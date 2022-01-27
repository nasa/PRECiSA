/**
 * @module PlotView
 * @author Paolo Masci
 * @date 2021.11.12
 * @copyright 
 * Copyright 2019 United States Government as represented by the Administrator 
 * of the National Aeronautics and Space Administration. All Rights Reserved.
 *
 * Disclaimers
 *
 * No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY
 * WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY,
 * INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE
 * WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM
 * INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR
 * FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO
 * THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER,
 * CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT
 * OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY
 * OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.
 * FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES
 * REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE,
 * AND DISTRIBUTES IT "AS IS."
 *
 * Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS
 * AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND
 * SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF
 * THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES,
 * EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM
 * PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT
 * SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED
 * STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
 * PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE
 * REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL
 * TERMINATION OF THIS AGREEMENT.
 **/

import * as path from "path";
import { ExtensionContext, Position, Uri, ViewColumn, WebviewPanel, WebviewPanelOnDidChangeViewStateEvent, window, workspace, WorkspaceEdit } from "vscode";
import { LanguageClient } from "vscode-languageclient";
import { PrecisaInputData, PrecisaAnalysisResponse, SensitivityAnalysisResponse, IntervalAnalysisResponse, PrecisaOptions, ComparativeAnalysisResponse, FormulaDesc, PrecisaAnalysisRequest, PrecisaComparativeAnalysisRequest, PrecisaSaveResultsRequest, PrecisaSaveResultsResponse, PrecisaServerCommands, PavingAnalysisResponse } from "../../common/precisa";
import { annotations, fpRangeRegex, matchingParens, precisaFunctionToString, precisaRangeToString, pvsIdentifier, rangeInfoRegex } from "../../common/precisaUtils";
import { showInformationMessage, showWarningMessage } from "../../utils/vscode-utils";
import { bodyTemplate, JSCommand, htmlTemplate, HtmlTemplate, JSDiv, TableTemplate, tableTemplate, rangeIn } from "./handlebarsTemplate";

import * as Handlebars from "handlebars";
import Backbone = require('backbone');
import { makeMesh3D, Mesh3DPlot, PavingCorners, VarRange, PavingData } from "../../common/paverUtils";

/**
 * Toolkit modes
 */
export enum Mode {
    // options
    options = "options",
    // analysis types
    errorBounds = "error-bounds",
    sensitivity = "sensitivity",
    intervals = "intervals",
    comparative = "comparative",
    poaving = "paving" // TODO
};

/**
 * Requests/Notifications exchanged with the webview
 */
 export enum PrecisaWebviewCommands {
    // analysis commands sent from vscode to the webview -- they are in the form compute-<Mode>
    computeErrorBounds = "compute-error-bounds",
    computeSensitivity = "compute-sensitivity",
    computeIntervals = "compute-intervals",
    computeComparative = "compute-comparative",
    computePaving = "compute-paving",
    // save results command sent from the webview to vscode
    saveResults = "save-results",
    // command sent to the webview to programmatically change toolkit mode
    changeMode = "change-mode"
};

/**
 * Message types
 */
 export declare interface WebviewRequest {
    command: PrecisaWebviewCommands,
    data?: PrecisaInputData[],
    options?: PrecisaOptions
}
export declare interface ErrorBoundsAnalysisRequest extends WebviewRequest {
    command: PrecisaWebviewCommands.computeErrorBounds
};
export declare interface SensitivityAnalysisRequest extends WebviewRequest {
    command: PrecisaWebviewCommands.computeSensitivity
};
export declare interface IntervalsAnalysisRequest extends WebviewRequest {
    command: PrecisaWebviewCommands.computeIntervals
};
export declare interface ComparativeAnalysisRequest extends WebviewRequest {
    command: PrecisaWebviewCommands.computeComparative
};
export declare interface ComputePavingRequest extends WebviewRequest {
    command: PrecisaWebviewCommands.computePaving
};
export type SaveResultsRequest = {
    command: PrecisaWebviewCommands.saveResults
};
export type ChangeModeRequest = {
    command: PrecisaWebviewCommands.changeMode,
    mode: Mode
};
export type PrecisaMessage = ErrorBoundsAnalysisRequest | SensitivityAnalysisRequest 
    | IntervalsAnalysisRequest | ComparativeAnalysisRequest | SaveResultsRequest 
    | ComputePavingRequest | PlotRequest | ChangeModeRequest | CompareData;

export enum PlotViewEvents {
    onDidChangeViewState = "onDidChangeViewState",
    onDidDispose = "onDidDispose"
};
export declare interface DidChangeViewStateEvent {
    view: ToolkitView
};
export declare interface DidDisposeEvent {
    view: ToolkitView
};
export interface WebViewCommand {
    command: JSCommand
}
export interface PlotRequest extends WebViewCommand {
    command: JSCommand.barPlot,
    data: PlotData[], // plot data
    title: string,    // plot title
    div?: JSDiv,      // plot div name
    yaxis?: string,   // x axis label (default: none)
    xaxis?: string,   // y axis label (default: Error)
    showxlabels?: boolean, // show x tick labels (default: false)
    xrangefixed?: boolean, // prevent x drag/zoom (default: false)
    yrangefixed?: boolean, // prevent y drag/zoom (default: false)
    rows?: number,    // number of rows, for multiple plots on a grid
    cols?: number,    // number of cols, for multiple plots on a grid
    xlabelOverrides?: string | string[], // tick labels
    xlabelAppendCounter?: boolean        // whether a counter should be appended to tick labels (1 to first tick, 2 to the second tick, ....)
};
export interface Plot3DRequest extends WebViewCommand {
    command: JSCommand.plot3D,
    data: Mesh3DPlot[], // plot data
    title?: string,     // plot title
    axes?: { name: string, min: string, max: string }[]
    // xaxis?: string,     // x axis label
    // yaxis?: string      // y axis label
};
export interface PlotParallelRequest extends WebViewCommand {
    command: JSCommand.parallelPlot,
    data: ParallelPlotData[], // plot data
    maxticks: number,
    tickvals?: boolean,
    title?: string
};
export interface TableRequest extends WebViewCommand {
    command: JSCommand.table,
    data: string // table data
};
export interface CompareData extends WebViewCommand {
    command: JSCommand.compareData,
    fun: string
};
export interface SwitchTo extends WebViewCommand {
    command: JSCommand.switchTo,
    mode: Mode
};
export type PlotType = "bar" | "table";
export type PlotData = BarPlotData | ParallelPlotData;
export interface BarPlotData {
    type: "bar",
    orientation?: "v" | "h",
    x: string[],
    y: number[],
    name?: string,
    text?: string[],
    hovertext: string[],
    hoverinfo: string[],
    xhoverformat?: string, // see https://plotly.com/javascript/reference/ and https://github.com/d3/d3-format/tree/v1.4.5#d3-formatfor syntax
    yhoverformat?: string, // see https://plotly.com/javascript/reference/ and https://github.com/d3/d3-format/tree/v1.4.5#d3-format for syntax
    marker?: {
        color?: string | number[],
        colorscale?: "Blackbody" | "Bluered" | "Blues" | "Cividis" | "Earth" | "Electric" | "Greens" | "Greys"
            | "Hot" | "Jet" | "Picnic" | "Portland" | "Rainbow" | "RdBu" | "Reds" | "Viridis" | "YlGnBu" | "YlOrRd"
    }
};
export interface ParallelDimension {
    values: number[],
    tickvals?: number[],
    ticktext?: string[],
    range: [number, number],
    constraintrange?: [number, number],
    multiselect?: boolean,
    label: string,
    customdata?: any
};
export interface ParallelPlotData {
    type: "parcoords",
    line?: {
        colorscale?: "Blackbody" | "Bluered" | "Blues" | "Cividis" | "Earth" | "Electric" | "Greens" | "Greys"
            | "Hot" | "Jet" | "Picnic" | "Portland" | "Rainbow" | "RdBu" | "Reds" | "Viridis" | "YlGnBu" | "YlOrRd",
        color: number[] | string[] | string
    },
    // x: string[],
    // y: number[],
    name?: string,
    // text?: string[],
    // hovertext: string[],
    // hoverinfo: string[],
    xhoverformat?: string, // see https://plotly.com/javascript/reference/ and https://github.com/d3/d3-format/tree/v1.4.5#d3-formatfor syntax
    yhoverformat?: string, // see https://plotly.com/javascript/reference/ and https://github.com/d3/d3-format/tree/v1.4.5#d3-format for syntax
    dimensions?: ParallelDimension[],
    maxticks: number
};
export interface TableData {
    type: "table",
    header: {
        values: string[][]
    },
    cells: {
        values: string[][],
        align: "center",
        line: { color: "black", width: 1 },
        font: { family: "Arial", size: 10, color: [ "black" ] }
    }
};
export interface FpRange {
    [varName: string]: { min: string, max: string }
};

// useful constants
const MAX_INNER_LABEL_LEN: number = 128;
const MAX_TAB_LABEL_LEN: number = 24;
const DEFAULT_UNCERTAINTY: string = "1%";
const DEFAULT_INTERVALS: number = 10;

/**
 * Utility function, creates the x-axis label for given input data and options
 * The label is in the form: X in [ 0, 1 ], Y in [ 0, 1 ]
 */
export function makeLabel (desc: { input: PrecisaInputData[], errorBound: string, options: PrecisaOptions }, opt?: { prefix?: string }): string {
    let xLabel: string = "";
    for (let i = 0; i < desc?.input?.length; i++) {
        xLabel += desc.input[i].name + " in [" + desc.input[i].range.min + "," + desc.input[i].range.max + "]";
        if (i < desc.input.length - 1) { xLabel += ", "; }
    }
    xLabel += desc?.options ? ` (p=${desc.options.precision}, d=${desc.options.depth})` : "";
    if (opt?.prefix) { xLabel = `${opt.prefix} ${xLabel}`; }
    return xLabel;
}
/**
 * Utility function, creates plot data for given input data
 */
export function makeBarPlot (
    desc: { input: PrecisaInputData[], errorBound: string, options: PrecisaOptions }, 
    opt?: { prefix?: string, group?: string, type?: "bar" }
): BarPlotData {
    opt = opt || {};
    const xLabel: string = makeLabel(desc, opt);
    // const color: string = cssColors[colorID];
    // colorID = (colorID + 1) % cssColors.length;
    return desc?.input?.length ? {
        x: [ ` ${xLabel} ` ],
        y: [ +desc.errorBound ], 
        type: "bar",
        // marker: {
        //     colorscale: "Portland",
        //     color: [...Array(desc.input.length + 1).keys()]
        // },
        name: "",
        text: [ "" ],
        hovertext: [ Number.parseFloat(desc.errorBound).toExponential() ],
        hoverinfo: [ "x+text" ]
    } : {
        x: [ opt.group || "" ],
        y: [ 0 ], 
        type: "bar",
        name: "",
        text: [ "" ],
        hovertext: [ `` ],
        hoverinfo: [ "text" ]
    };
}
/**
 * Utility function, creates diff plot data for given input data
 */
export function makeDiffPlot (
    desc1: { input: PrecisaInputData[], errorBound: string, options: PrecisaOptions }, 
    desc2: { input: PrecisaInputData[], errorBound: string, options: PrecisaOptions }, 
    opt?: { prefix?: string, group?: string, type?: "bar" }
): BarPlotData {
    opt = opt || {};
    const xLabel: string = "diff";
    const diff: number = Math.abs(+desc1.errorBound - +desc2.errorBound);
    return desc1?.input?.length && desc2?.input?.length && desc1?.input?.length === desc2?.input?.length ? {
        x: [ ` ${xLabel} ` ],
        y: [ diff ], 
        type: "bar",
        // marker: {
        //     colorscale: "Portland",
        //     color: [...Array(desc.input.length + 1).keys()]
        // },
        name: "",
        text: [ "" ],
        hovertext: [ Number.parseFloat(`${diff}`).toExponential() ],
        hoverinfo: [ "x+text" ]
    } : {
        x: [ opt.group || "" ],
        y: [ 0 ], 
        type: "bar",
        name: "",
        text: [ "" ],
        hovertext: [ `` ],
        hoverinfo: [ "text" ]
    };
}
/**
 * Make parallel coordinates plot
 */
export function makeParallelPlot (
    desc: { corners: PavingCorners[], variables: string[], input: PrecisaInputData[] }, 
    opt?: { tickvals?: boolean, fullrange?: boolean }
): ParallelPlotData {
    opt = opt || {};
    const traces: ParallelDimension[] = [];
    let maxticks: number = desc?.corners?.length;
    // first col contains labels
    const tickvals: number[] = [];
    let values: number[] = [];
    for (let i = 0; i < desc?.corners?.length; i++) {
        values = values.concat([ i, i ]);
        tickvals.push(i);
    }
    const selected: number = parseInt(`${tickvals.length / 2}`); // see parallel-plot case in domTemplate
    traces.push({
        values,
        // tickvals,
        range: [ 0, tickvals.length ],
        constraintrange: [ selected + 0.5, selected - 0.5 ],
        multiselect: false,
        label: "Box #"
    });
    // other cols contains paving data
    for (let v = 0; v < desc?.variables?.length; v++) {
        // Create a trace for each variable.
        // The trace is obtained by the concatenating the paving corners
        const trace: ParallelDimension = {
            values: [ ],
            tickvals: [ ],
            ticktext: [ ],
            range: [ +desc.input[v]?.range?.min, +desc.input[v]?.range?.max ],
            label: desc.variables[v]
        };
        const tickvals: { [key: string]: string } = {};
        for (let i = 0; i < desc?.corners?.length; i++) {
            const corner: VarRange = desc.corners[i][v];
            trace.values = trace.values.concat([ +corner[0], +corner[1] ]);
            tickvals[corner[0]] = corner[0];
            tickvals[corner[1]] = corner[1];
        }
        const keys: string[] = Object.keys(tickvals);
        for (let i = 0; i < keys.length; i++) {
            trace.tickvals.push(+keys[i]);
            trace.ticktext.push(opt.tickvals ? keys[i] : "");
        }
        if (!opt?.fullrange) {
            const min: number = Math.min(...trace.values);
            const max: number = Math.max(...trace.values);
            trace.range = [ min, max ];
        }
        if (v < desc.input?.length) {
            trace.label += ` ${rangeIn} [${trace.range[0]}, ${trace.range[1]}]`;
        }
        // trace.customdata = { values: trace.values }; // this does not seem to be working
        traces.push(trace);
        maxticks = desc?.corners?.length > maxticks ? desc.corners.length : maxticks;
    }
    return {
        // x: [ ` ${xLabel} ` ],
        // y: [ 0 ], 
        type: "parcoords",
        line: {
            // colorscale: "Bluered", // "Jet"
            // color: [ ...Array(desc.corners.length * 2) ]//.keys() ]
            color: "red"
        },
        name: "",
        // text: [ "" ],
        dimensions: traces,
        maxticks
        // [
        //     {
        //     values: [0,0, 1,1, 2,2, 3,3, 4,4],
        //     tickvals: [0,1,2,3,4,5,6],
        //     ticktext: ['test 1','test 2','test 3','test 4', 'test 5'],
        //     range: [0, 6],
        //     constraintrange: [0.5, 1.5],
        //     label: 'test cases',
        // },{
        //     values: [0,3, 3,6, 6,9, 9,12, 12,15],
        //     tickvals: [0, 3, 6, 9, 12, 15],
        //     range: [0, 20],
        //     label: 'x',
        // }, {    
        //     values: [0,3, 0,3, 0,3, 0,3, 0,3],
        //     tickvals: [0, 3],
        //     range: [0, 20],
        //     label: 'y',
        // }]
    };
}
/**
 * Utility function, creates empty plot data, useful for spacing between groups
 */
export function makeSpacer (group?: string): BarPlotData {
    return makeBarPlot(null, { group })
}
/**
 * Utility function, creates a table for given input data
 */
export function makeHtmlTable (desc: {
    col1: { [xLabel: string]: BarPlotData },
    col2?: { [xLabel: string]: BarPlotData },
    col3?: { [xLabel: string]: BarPlotData },
    header?: string[]
}, opt?: {
    labels?: { [xLabel: string]: string }
}): string {
    if (desc) {
        opt = opt || {};
        const rows: { cols: string[] }[] = [];
        const keys: string[] = Object.keys(desc.col1);
        for (let i = 0; i < keys.length; i++) {
            const val1: PlotData = desc.col1[keys[i]];
            const val2: PlotData = desc?.col2 ? desc.col2[keys[i]] : null;
            const val3: PlotData = desc?.col3 ? desc.col3[keys[i]] : null;
            const col0: string = opt.labels ? opt.labels[keys[i]] : keys[i]; //`Experiment ${i + 1}`;
            const col1: string = val1?.hovertext?.length ? val1.hovertext[0] : "";
            const col2: string = val2?.hovertext?.length ? val2.hovertext[0] : null;
            const col3: string = val3?.hovertext?.length ? val3.hovertext[0] : null;
            rows.push(col3 ? { cols: [ col0, col1, col2, col3 ] }
                : col2 ? { cols: [ col0, col1, col2 ] } 
                    : { cols: [ col0, col1 ] });
        }
        const header: string[] = desc.header;
        const tableData: TableTemplate = { header, rows };
        const htmlContent: string = Handlebars.compile(tableTemplate, { noEscape: true })(tableData);
        return htmlContent;
    }
    return "";
}
/**
 * Utility function, creates a multicolumn table for given input data
 */
export function makeHtmlTableMulti (desc: {
    paving: PavingData,
    header?: string[]
}): string {
    if (desc?.paving) {
        const rows: { cols: string[] }[] = [];
        const keys: string[] = desc.paving.variables;
        const possibly: PavingCorners[] = desc.paving.possibly || [];
        const certainly: PavingCorners[] = desc.paving.certainly || [];
        const data: PavingCorners[] = possibly.concat(certainly);
        for (let c = 0; c < data.length; c++) {
            let cols = [ `#${c}` ];
            for (let i = 0; i < keys.length; i++) {
                const v: string = keys[i];
                const range: VarRange = data[c][i];
                if (range?.length > 1) {
                    const min: string = range[0];
                    const max: string = range[1];
                    cols.push(`${v} in [${min},${max}]`);
                }
            }
            rows.push({ cols });
        }
        const header: string[] = desc.header;
        const tableData: TableTemplate = { header, rows };
        const htmlContent: string = Handlebars.compile(tableTemplate, { noEscape: true })(tableData);
        return htmlContent;
    }
    return "";
}


/**
 * Toolkit view
 */
export class ToolkitView extends Backbone.Model {
    // client for interacting with the server back-end
    protected client: LanguageClient
    // webview panel
    protected pane: WebviewPanel;
    // client context
    protected context: ExtensionContext;

    // last result
    protected lastResult: PrecisaAnalysisResponse;
    // function args
    protected args: PrecisaInputData[];
    // current function
    protected fun: FormulaDesc;
    // current comparisong
    protected compareFun: FormulaDesc;
    // mode currently active in the view
    protected mode: Mode = Mode.errorBounds;

    // plots created during this session
    protected errorBoundsPlot: { [xLabel: string]: BarPlotData } = { }; // error bound
    protected instabilityPlot: { [xLabel: string]: BarPlotData } = { }; // instability
    protected comparativePlot: { [xLabel: string]: BarPlotData[] } = { }; // comparative

    /**
     * Constructor
     */
    constructor (client: LanguageClient) {
        super();
        this.client = client;
    }

    /**
     * Activates the module
     */
    activate (context: ExtensionContext) {
        this.context = context;
    }

    /**
     * Internal function, creates the html content of the webview
     */
    protected createContent (desc: FormulaDesc): string {
        // generate title based on formula name and args
        const functionName: string = precisaFunctionToString({ formula: desc.formula, input: this.args });
        const title: string = `PRECiSA Floating Point Error Analysis ~ ${functionName}`;
        // get js and css files
        const bootstrapJsOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/js/bootstrap.bundle.min.js'));
        const bootstrapCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/bootstrap/dist/css/bootstrap.min.css'));
        const jqueryOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/jquery/dist/jquery.min.js'));
        const fontawesomeCssOnDisk: Uri = Uri.file(path.join(this.context.extensionPath, 'client/node_modules/font-awesome/css/font-awesome.min.css'));
        const css = [
            this.pane.webview.asWebviewUri(bootstrapCssOnDisk),
            this.pane.webview.asWebviewUri(fontawesomeCssOnDisk)
        ];
        const js = [
            this.pane.webview.asWebviewUri(jqueryOnDisk), // jquery needs to be loaded before bootstrap
            this.pane.webview.asWebviewUri(bootstrapJsOnDisk)
        ];
        // generate body
        const body: string = Handlebars.compile(bodyTemplate, { noEscape: true })({
            inputData: this.args,
            functionName
        });
        // generate html
        const data: HtmlTemplate = { title, js, css, body };
        const html: string = Handlebars.compile(htmlTemplate, { noEscape: true })(data);
        return html;
    }
    /**
     * Internal function, processes annotation tag @fp-range
     */
    getFpRange (annotation: string): FpRange {
        if (annotation) {
            // search @fp-range in the annotation
            const rangeRegex: RegExp = new RegExp(fpRangeRegex);
            const matchRange: RegExpMatchArray = rangeRegex.exec(annotation);
            if (matchRange?.length > 1) {
                const data: string = matchRange[1];
                const regexInfo: RegExp = new RegExp(rangeInfoRegex);
                let matchInfo: RegExpMatchArray = null;
                const ans: FpRange = {};
                while (matchInfo = regexInfo.exec(data)) {
                    // group 1 is the variable name
                    // group 2 is the variable range
                    if (matchInfo?.length > 2) {
                        const varName: string = matchInfo[1];
                        const varRange: string[] = matchInfo[2].split(",");
                        if (varRange?.length === 2) {
                            ans[varName] = { min: varRange[0].trim(), max: varRange[1].trim() };
                        }
                    }
                }
                return ans;
            }
        }
        return null;
    }
    /**
     * Render panel
     */
    async render (desc: FormulaDesc): Promise<boolean> {
        return new Promise((resolve, reject) => {
            if (desc?.formula) {
                this.matchInputData(desc);
                const formula: string = precisaFunctionToString({ formula: desc.formula, input: this.args });
                const label: string = formula.length > MAX_INNER_LABEL_LEN ? formula.substring(0, MAX_TAB_LABEL_LEN) + "..." : formula;
                // create webview if needed
                if (!this.pane) {
                    this.pane = window.createWebviewPanel(
                        'vscode-precisa.plot-view', // Identifies the type of the webview. Used internally
                        `Function ${label}`, // Title of the panel displayed to the user
                        ViewColumn.Beside, // Editor column to show the new webview panel in.
                        {
                            enableScripts: true,
                            retainContextWhenHidden: true
                        } // Webview options.
                    );
                    // set panel icon
                    this.pane.iconPath = {
                        light: Uri.file(path.join(__dirname, "..", "..", "..", "..", "icons", "precisa-file-icon-large.png")),
                        dark: Uri.file(path.join(__dirname, "..", "..", "..", "..", "icons", "precisa-file-icon-large.png"))
                    };
                    // set function descriptor
                    this.fun = desc;
                    // set webview content
                    this.pane.webview.html = this.createContent(desc);
                    // install handlers
                    // Handle messages from the webview
                    this.pane.webview.onDidReceiveMessage(
                        async (message: PrecisaMessage) => {
                            switch (message?.command) {
                                case PrecisaWebviewCommands.computeErrorBounds: {
                                    // send request to the server
                                    const input: PrecisaInputData[] = <PrecisaInputData[]> message.data;
                                    const options: PrecisaOptions = <PrecisaOptions> message.options;
                                    await this.sendErrorBoundAnalysisRequest(input, options);
                                    break;
                                }
                                case PrecisaWebviewCommands.computeSensitivity: {
                                    // send request to the server
                                    const input: PrecisaInputData[] = <PrecisaInputData[]> message.data;
                                    const options: PrecisaOptions = <PrecisaOptions> message.options;
                                    await this.sendSensitivityAnalysisRequest(input, options);
                                    break;
                                }
                                case PrecisaWebviewCommands.computeIntervals: {
                                    // send request to the server
                                    const input: PrecisaInputData[] = <PrecisaInputData[]> message.data;
                                    const options: PrecisaOptions = <PrecisaOptions> message.options;
                                    await this.sendIntervalAnalysisRequest(input, options);
                                    break;
                                }
                                case PrecisaWebviewCommands.computeComparative: {
                                    // send request to the server
                                    const input: PrecisaInputData[] = <PrecisaInputData[]> message.data;
                                    const options: PrecisaOptions = <PrecisaOptions> message.options;
                                    await this.sendComparativeAnalysisRequest(input, options);
                                    break;
                                }
                                case PrecisaWebviewCommands.computePaving: {
                                    // send request to the server
                                    const input: PrecisaInputData[] = <PrecisaInputData[]> message.data;
                                    const options: PrecisaOptions = <PrecisaOptions> message.options;
                                    await this.sendPavingAnalysisRequest(input, options);
                                    break;
                                }
                                case PrecisaWebviewCommands.saveResults: {
                                    await this.sendSaveResultsRequest(desc);
                                    break;
                                }
                                case PrecisaWebviewCommands.changeMode: {
                                    // sanity check
                                    if (message.mode === Mode.comparative || message.mode === Mode.errorBounds 
                                            || message.mode === Mode.intervals || message.mode === Mode.options 
                                            || message.mode === Mode.sensitivity) {
                                        this.mode = message.mode;
                                    } else {
                                        console.warn(`[precisa-analysis-view] Warning: unknown mode`, message);
                                    }
                                    break;
                                }
                                default: {
                                    console.log("[precisa-analysis-view] Warning: unknown analysis request", message);
                                    break;
                                }
                            }
                        }
                    );
                    this.pane.onDidDispose(() => {
                        // dispose resources
                        this.clearPlots();
                        const desc: DidChangeViewStateEvent = { view: this };
                        this.trigger(PlotViewEvents.onDidDispose, desc);
                    });
                    this.pane.onDidChangeViewState((evt: WebviewPanelOnDidChangeViewStateEvent) => {
                        const pane: WebviewPanel = evt?.webviewPanel;
                        if (pane === this.pane) {
                            const desc: DidChangeViewStateEvent = { view: this };
                            this.trigger(PlotViewEvents.onDidChangeViewState, desc);
                        }
                    });
                }
                // done!
                resolve(true);
            } else {
                resolve(false);
            }
        });
    }
    /**
     * Switch the webview to a given mode
     */
    switchTo (mode: Mode): void {
        const message: SwitchTo = {
            command: JSCommand.switchTo,
            mode
        };
        this.pane?.webview?.postMessage(message);
    }
    /**
     * Focus the panel
     */
    focus (): void {
        this.pane?.reveal();
    }
    /**
     * Send compare request to the webview
     */
    async compareWith (desc: FormulaDesc, opt?: { confirm?: boolean }): Promise<boolean> {
        opt = opt || {};
        const formula: string = precisaFunctionToString({ formula: desc.formula, input: this.args });
        // ask confirmation
        const msg: string = `Compare ${formula} with ${precisaFunctionToString({ formula: this.fun.formula, input: this.args })}?`;
        const yesno: string[] = [ "Yes", "No" ];
        const ans: string = opt.confirm ? 
            await window.showInformationMessage(msg, { modal: true }, yesno[0])
                : yesno[0];
        if (ans === yesno[0]) {
            this.compareFun = desc;
            const message: CompareData = {
                command: JSCommand.compareData,
                fun: `${formula}`
            };
            this.pane?.webview?.postMessage(message);
            return true;
        }
        return false;
    }
    /**
     * Sends a save request to the server
     */
    async sendSaveResultsRequest (desc: FormulaDesc): Promise<boolean> {
        return new Promise ((resolve, reject) => {
            const req: PrecisaSaveResultsRequest = {
                ...desc,
                results: this.lastResult
            };
            this.client.sendRequest(PrecisaServerCommands.saveResults, req);
            this.client.onNotification(PrecisaServerCommands.saveResults, async (desc: { req: PrecisaSaveResultsRequest, res: PrecisaSaveResultsResponse }) => {
                if (desc?.res?.success) {
                    const success: boolean = await this.annotatePvsFile(this.lastResult);
                    success ? showInformationMessage(`Annotation saved in the pvs file.`, { timeout: 8000 })
                        : showWarningMessage(`Unable to save annotation, please check Output view for additional information.`);
                    resolve(success);
                }
            });    
        })
    }
    /**
     * Sends a compare request to the server
     */
    async sendComparativeAnalysisRequest (input: PrecisaInputData[], options: PrecisaOptions): Promise<boolean> {
        return new Promise ((resolve, reject) => {
            // sanity check
            if (this.compareFun?.formula) {
                // send request to the server
                const compared: PrecisaAnalysisRequest = {
                    ...this.compareFun,
                    input,
                    options
                    // TODO
                };
                const req: PrecisaComparativeAnalysisRequest = {
                    ...this.fun,
                    input,
                    options,
                    compareWith: compared
                };
                this.client.sendRequest(PrecisaServerCommands.computeComparative, req);
                this.client.onNotification(PrecisaServerCommands.computeComparative, (desc: { req: PrecisaComparativeAnalysisRequest, res: ComparativeAnalysisResponse }) => {
                    const success: boolean = !!desc?.res;
                    if (success) {
                        this.plotComparative(desc.res);
                        // create table
                        const keys: string[] = Object.keys(this.comparativePlot);
                        const col1: { [xLabel: string]: BarPlotData } = {};
                        const col2: { [xLabel: string]: BarPlotData } = {};
                        const col3: { [xLabel: string]: BarPlotData } = {}; // col3 contains the diff
                        for (let i = 0; i < keys?.length; i++) {
                            const plot1: BarPlotData = this.comparativePlot[keys[i]]?.length ? this.comparativePlot[keys[i]][0] : null;
                            const plot2: BarPlotData = this.comparativePlot[keys[i]]?.length > 1 ? this.comparativePlot[keys[i]][1] : null;
                            const plot3: BarPlotData = this.comparativePlot[keys[i]]?.length > 2 ? this.comparativePlot[keys[i]][2] : null;
                            const x1Label: string = plot1.x?.length ? plot1.x[0] : `${i}`;
                            const x2Label: string = plot2.x?.length ? plot1.x[0] : `${i}`;
                            const xLabel: string = x1Label.split(":")[0] + " vs. " + x2Label;
                            col1[xLabel] = plot1;
                            col2[xLabel] = plot2;
                            col3[xLabel] = plot3;
                        }
                        const f1: string = precisaFunctionToString({ formula: desc.req.formula, input: desc.req.input });
                        const f2: string = precisaFunctionToString({ formula: desc.req.compareWith?.formula, input: desc.req.input });
                        this.createTable({
                            col1,
                            col2,
                            col3,
                            header: [ "Experiment", f1, f2, "diff" ]
                        });
                    }
                    resolve(success)
                });
            } else {
                console.warn("[plot-view] Warning: requested comparative analysis but compare fun is null");
            }
        });
    }
    /**
     * Sends interval analysis request to the server
     */
    async sendIntervalAnalysisRequest (input: PrecisaInputData[], options: PrecisaOptions): Promise<boolean> {
        return new Promise ((resolve, reject) => {
            const req: PrecisaAnalysisRequest = {
                ...this.fun,
                input,
                options
            };
            this.client.sendRequest(PrecisaServerCommands.computeIntervals, req);
            this.client.onNotification(PrecisaServerCommands.computeIntervals, (desc: { req: PrecisaAnalysisRequest, res: IntervalAnalysisResponse }) => {
                const success: boolean = !!desc?.res;
                if (success) {
                    const data: BarPlotData[] = this.plotIntervals(desc.res);
                    // create table
                    const col1: { [xLabel: string]: BarPlotData } = {};
                    for (let i = 0; i < data?.length; i++) {
                        const xLabel: string = data[i].x?.length ? data[i].x[0] : `${i}`;
                        col1[xLabel] = data[i];
                    }
                    this.createTable({
                        col1,
                        header: [ "Experiment", "Accumulated Round-Off Error" ]
                    });
                }
                resolve (success);
            });
        });
    }
    /**
     * Sends interval analysis request to the server
     */
    async sendSensitivityAnalysisRequest (input: PrecisaInputData[], options: PrecisaOptions): Promise<boolean> {
        return new Promise ((resolve, reject) => {
            const req: PrecisaAnalysisRequest = {
                ...this.fun,
                input,
                options
            };
            this.client.sendRequest(PrecisaServerCommands.computeSensitivity, req);
            this.client.onNotification(PrecisaServerCommands.computeSensitivity, (desc: { req: PrecisaAnalysisRequest, res: SensitivityAnalysisResponse }) => {
                const success: boolean = !!desc?.res;
                if (success) {
                    const data: BarPlotData[] = this.plotSensitivity(desc.res);
                    // create table
                    const col1: { [xLabel: string]: BarPlotData } = {};
                    for (let i = 0; i < data?.length; i++) {
                        const xLabel: string = data[i].x?.length ? data[i].x[0] : `${i}`;
                        col1[xLabel] = data[i];
                    }
                    this.createTable({
                        col1,
                        header: [ "Experiment", "Accumulated Round-Off Error" ]
                    });
                }
                resolve(success)
            });
        });
    }
    /**
     * Sends error bound analysis request to the server
     */
    async sendErrorBoundAnalysisRequest (input: PrecisaInputData[], options: PrecisaOptions): Promise<boolean> {
        return new Promise ((resolve, reject) => {
            const req: PrecisaAnalysisRequest = {
                ...this.fun,
                input,
                options
            };
            this.client.sendRequest(PrecisaServerCommands.computeErrorBounds, req);
            this.client.onNotification(PrecisaServerCommands.computeErrorBounds, (desc: { req: PrecisaAnalysisRequest, res: PrecisaAnalysisResponse }) => {
                const success: boolean = !!desc?.res;
                if (success) {
                    this.lastResult = desc.res;
                    this.plotErrorBound(desc.res);
                    this.plotInstability(desc.res);
                    this.createTable({
                        col1: this.errorBoundsPlot,
                        col2: this.instabilityPlot,
                        header: Object.keys(this.instabilityPlot)?.length ?
                            [ "Experiment", "Accumulated Round-Off Error", "Instability" ]
                                : [ "Experiment", "Accumulated Round-Off Error" ]
                    });
                }
                resolve (success);
            });
        });
    }
    /**
     * Sends a compare request to the server
     */
    async sendPavingAnalysisRequest (input: PrecisaInputData[], options: PrecisaOptions): Promise<boolean> {
        return new Promise ((resolve, reject) => {
            const req: PrecisaAnalysisRequest = {
                ...this.fun,
                input,
                options
            };
            this.client.sendRequest(PrecisaServerCommands.computePaving, req);
            this.client.onNotification(PrecisaServerCommands.computePaving, (desc: { req: PrecisaAnalysisRequest, res: PavingAnalysisResponse }) => {
                const success: boolean = !!desc?.res;
                if (success) {
                    // create table
                    const names: string[] = desc.res?.paving?.variables || []
                    this.createTableMulti({
                        paving: desc.res?.paving,
                        header: [ "Box", ...names ]
                    });
                    // create plots
                    this.plotPaving(desc.res, { fullrange: true, selected: options?.selected });
                }
                resolve (success);
            });
        });
    }

    /**
     * Annotate the pvs file with the given error bound info
     */
    async annotatePvsFile (desc: PrecisaAnalysisResponse): Promise<boolean> {
        if (!desc) { return false; }
        return new Promise((resolve, reject) => {
            const edit = new WorkspaceEdit();
            const uri: Uri = Uri.file(path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`));
            const position: Position = new Position(desc.position.line, desc.position.character);
            // example annotation
            // % @fp-error 5.773159728050815e-15 (instability 2.1000244140625007e+1) when X in [ 2, 4 ], Y in [ 5, 5 ] (p=7, d=14)
            const message: string = `% ${annotations.fp_error} ${Number.parseFloat(desc.errorBound).toExponential()}`
                + `${desc.unstableErrorBound ? " (instability " + Number.parseFloat(desc.unstableErrorBound).toExponential() + ")" : ""}`
                + ` when ${precisaRangeToString(desc)} (p=${desc.options.precision}, d=${desc.options.depth})\n`;
            // TODO: check if we want to remove previous annotations
            edit.insert(uri, position, message);
            workspace.applyEdit(edit).then((success: boolean) => {
                resolve(success);
            }, (error) => {
                console.log("[precisa-analysis-view] Warning: unable to annotate pvs file", error, desc);
                resolve(false);
            });
        })
    }

    /**
     * Returns input data of a given formula descriptor
     */
    protected matchInputData (desc: FormulaDesc): void {
        this.args = [];
        // get args
        const frag: string = desc?.theory.slice(desc?.position?.index + desc?.annotation?.length);
        if (frag) {
            const range: FpRange = this.getFpRange(desc?.annotation) || {};
            const idx: { start: number, end: number } = matchingParens(frag);
            if (idx) {
                const args: string = frag.slice(idx.start + 1, idx.end);
                const argv: string[] = args.split(",");
                const varNames: string[] = argv.map((elem: string) => {
                    return elem.split(":")[0].trim();
                });
                for (let i = 0; i < varNames.length; i++) {
                    const name: string = varNames[i];
                    // check if the varname is a valid name
                    const regex: RegExp = new RegExp(pvsIdentifier);
                    const match: RegExpExecArray = regex.exec(name);
                    const valid: boolean = match?.length && match[0].length === name.length;
                    if (valid) {
                        const min: string = range[name]?.min || "0";
                        const max: string = range[name]?.max || "1";
                        this.args.push({
                            name, 
                            range: {
                                min, 
                                max, 
                                uncertainty: `${DEFAULT_UNCERTAINTY}`,
                                intervals: `${DEFAULT_INTERVALS}`
                            }
                        });
                    }
                }
            }
        }
    }

    /**
     * Clears all stored tables and diagrams
     */
    clearPlots (): void {
        this.errorBoundsPlot = {};
        this.instabilityPlot = {};
        this.comparativePlot = {};
    }
    /**
     * Plot error bound
     */
    plotErrorBound (desc: PrecisaAnalysisResponse): void {
        if (desc?.input?.length) {
            const dt: PlotData = makeBarPlot(desc);
            const xLabel: string = makeLabel(desc);
            this.errorBoundsPlot[xLabel] = dt;
            const data: BarPlotData[] = Object.values(this.errorBoundsPlot);
            const message: PlotRequest = {
                command: JSCommand.barPlot, 
                data, 
                title: "Accumulated Round-Off Error", 
                div: JSDiv.plotErrorBounds,
                showxlabels: false,
                xaxis: "Experiments",
                xrangefixed: true,
                yrangefixed: true,
                xlabelAppendCounter: true,
                xlabelOverrides: " Experiment"
            };
            this.pane?.webview?.postMessage(message);
        }
    }
    /**
     * Plot error bound
     */
    plotInstability (desc: PrecisaAnalysisResponse): void {
        if (desc?.input?.length && desc.unstableErrorBound) {
            const dt: PlotData = makeBarPlot({
                input: desc.input,
                errorBound: desc.unstableErrorBound,
                options: desc.options
            });
            const xLabel: string = makeLabel(desc);
            this.instabilityPlot[xLabel] = dt;
            const message: PlotRequest = {
                command: JSCommand.barPlot, 
                data: Object.values(this.instabilityPlot), 
                title: "Overall floating-point error due to unstable guards",
                div: JSDiv.plotUnstable,
                showxlabels: false,
                xaxis: "Experiments",
                xrangefixed: true,
                yrangefixed: true,
                xlabelAppendCounter: true,
                xlabelOverrides: " Experiment"
            };
            this.pane?.webview?.postMessage(message);
        }
    }
    /**
     * Plot sensitivity
     */
    plotSensitivity (desc: SensitivityAnalysisResponse): BarPlotData[] {
        if (desc?.input?.length) {
            const data: BarPlotData[] = [ makeBarPlot(desc) ];
            const prefix: string = ""; //`(${desc.input[0].range?.uncertainty})`;
            if (desc.sensitivity?.length) {
                let group: string = desc.sensitivity[0].group;
                // introduce a gap in the diagram, to separate different groups of results
                data.push(makeSpacer(group));
                // plot all data
                for (let i = 0; i < desc.sensitivity.length; i++) {
                    // insert spacer if the group name changes
                    if (group !== desc.sensitivity[i].group) {
                        // update group and add gap in the diagram to separate the group
                        group = desc.sensitivity[i].group;
                        data.push(makeSpacer(group));
                    }
                    data.push(makeBarPlot({ ...desc.sensitivity[i], options: desc.options }, { prefix }));
                }
            }
            // for now, uncertainty is the same for all inputs
            const message: PlotRequest = { 
                command: JSCommand.barPlot, 
                data, 
                title: `Sensitivity Analysis Results (±${desc.input[0].range.uncertainty})`, 
                div: JSDiv.plotSensitivity,
                xrangefixed: true,
                yrangefixed: true,
                xaxis: "Experiments"
            };
            this.pane?.webview?.postMessage(message);
            return data;
        }
        return null;
    }
    /**
     * Plot intervals
     */
    plotIntervals (desc: IntervalAnalysisResponse): BarPlotData[] {
        if (desc?.input?.length) {
            const data: BarPlotData[] = [ makeBarPlot(desc) ];
            const prefix: string = ""; //`(${desc.input[0].range?.intervals})`;
            if (desc.intervals?.length) {
                let group: string = desc.intervals[0].group;
                // introduce a gap in the diagram, to separate different groups of results
                data.push(makeSpacer(group));
                // plot all intervals
                for (let i = 0; i < desc.intervals?.length; i++) {
                    // insert spacer if the group name changes
                    if (group !== desc.intervals[i].group) {
                        // update group and add gap in the diagram to separate the group
                        group = desc.intervals[i].group;
                        data.push(makeSpacer(group));
                    }
                    data.push(makeBarPlot({ ...desc.intervals[i], options: desc.options }, { prefix }));
                }
            }
            // for now, intervals is the same for all inputs
            const message: PlotRequest = { 
                command: JSCommand.barPlot, 
                data, 
                title: `Interval Analysis Results (${desc.input[0].range.intervals} intervals)`, 
                div: JSDiv.plotIntervals,
                xrangefixed: true,
                yrangefixed: true,
                xaxis: "Experiments"
            };
            this.pane?.webview?.postMessage(message);
            return data;
        }
        return null;
    }
    /**
     * Plot comparative
     */
    plotComparative (desc: ComparativeAnalysisResponse): BarPlotData[] {
        if (desc) {
            const label1: string = precisaFunctionToString(desc);
            const data: BarPlotData[] = [ makeBarPlot(desc, { prefix: `${label1}: ` }) ];
            const xLabel: string = makeLabel({ ...desc, options: desc.options });
            // const prefix: string = `(${desc.input[0].range?.intervals})`;
            const label2: string = ` ${precisaFunctionToString({ ...desc.compareWith, input: desc.input })}` // this label is made intentionally a bit different that the other, to make sure the plot has two x ticks
            const label3: string = "diff";
            if (desc.compared && desc.compareWith) {
                // plot compared
                data.push(makeBarPlot({
                    ...desc.compared, 
                    options: desc.options
                }, { 
                    prefix: `${label2}: `
                }));
                // plot diff
                data.push(makeDiffPlot(desc, {
                    ...desc.compared, 
                    options: desc.options
                }, { 
                    prefix: `${label3}`
                }))
            }
            this.comparativePlot[xLabel] = data;
            // plot current and history
            const keys: string[] = Object.keys(this.comparativePlot);
            let plotData: BarPlotData[] = [];
            for (let i = 0; i < keys.length; i++) {
                if (this.comparativePlot[keys[i]].length > 1) {
                    plotData = plotData.concat([
                        this.comparativePlot[keys[i]][0], 
                        this.comparativePlot[keys[i]][1]
                    ]);
                    if (i < keys.length - 1) {
                        // insert spacer
                        plotData = plotData.concat([ makeSpacer(`${i}`) ]);
                    }
                }
            }
            const message: PlotRequest = {
                command: JSCommand.barPlot, 
                data: plotData,
                title: `Comparative Analysis Results`,
                div: JSDiv.plotComparative,
                showxlabels: false,
                xaxis: "Experiments",
                xlabelOverrides: [ label1, label2 ],
                xrangefixed: true,
                yrangefixed: true
            };
            this.pane?.webview?.postMessage(message);
            return data;
        }
        return null;
    }
    /**
     * Plot paving
     */
    plotPaving (desc: PavingAnalysisResponse, opt?: { 
        tickvals?: boolean, 
        fullrange?: boolean,
        selected?: string[]
    }): void {
        // this.plotParallelPaving(desc, opt);
        this.plotMesh3DPaving(desc, opt);
    }
    /**
     * Plot mesh3D paving, can be used only when the function has 2 input variables
     */
    plotMesh3DPaving (desc: PavingAnalysisResponse, opt?: { selected?: string[] }): Mesh3DPlot[] {
        if (desc?.paving) {
            opt = opt || {};
            // 3d plot
            const height: number = 0;
            const plotData: Mesh3DPlot[] = [];
            const possibly: PavingCorners[] = desc.paving?.possibly || [];
            const index0: number = opt.selected.length && desc.paving.variables.includes(opt.selected[0]) ?
                desc.paving.variables.indexOf(opt.selected[0])
                    : 0;
            const index1: number = opt.selected.length > 1 && desc.paving.variables.includes(opt.selected[1]) ?
                desc.paving.variables.indexOf(opt.selected[1])
                    : 1;
            for (let i = 0; i < possibly.length && index0 < possibly[0].length && index1 < possibly[0].length; i++) {
                const corners: PavingCorners = possibly[i];
                const mesh: Mesh3DPlot = makeMesh3D([ corners[index0], corners[index1] ], height, "red");
                plotData.push(mesh);
            }
            const certainly: PavingCorners[] = desc.paving?.certainly || [];
            for (let i = 0; i < certainly.length && index0 < certainly[0].length && index1 < certainly[0].length; i++) {
                const corners: PavingCorners = certainly[i];
                const mesh: Mesh3DPlot = makeMesh3D([ corners[index0], corners[index1] ], height, "red");
                plotData.push(mesh);
            }

            // const almost_certainly: PavingCorners[] = desc.paving?.almost_certainly || [];
            // for (let i = 0; i < almost_certainly.length; i++) {
            //     const corners: PavingCorners = almost_certainly[i];
            //     const height: number = 0;
            //     const mesh: Mesh3DPlot = makeMesh3D(corners, height, "blue");
            //     plotData.push(mesh);
            // }
            // const certainly_not: PavingCorners[] = desc.paving?.certainly_not || [];
            // for (let i = 0; i < certainly_not.length; i++) {
            //     const corners: PavingCorners = certainly_not[i];
            //     const height: number = 0;
            //     const mesh: Mesh3DPlot = makeMesh3D(corners, height, "gray");
            //     plotData.push(mesh);
            // }

            // the following is an optimization -- all boxes are joined together into a larger box
            // const xtrace_min: number[] = almost_certainly.concat(certainly_not).map(elem => {
            //     return +elem[0][0];
            // });
            // const xtrace_max: number[] = almost_certainly.concat(certainly_not).map(elem => {
            //     return +elem[0][1];
            // });
            // const ytrace_min: number[] = almost_certainly.concat(certainly_not).map(elem => {
            //     return +elem[1][0];
            // });
            // const ytrace_max: number[] = almost_certainly.concat(certainly_not).map(elem => {
            //     return +elem[1][1];
            // });
            // const xbox: VarRange = [ `${Math.min(...xtrace_min)}`, `${Math.max(...xtrace_max)}` ];
            // const ybox: VarRange = [ `${Math.min(...ytrace_min)}`, `${Math.max(...ytrace_max)}` ];

            // the input range is used to create a gray surface that gives spatial information about the range of values
            const xbox: VarRange = desc.input?.length > index0 && desc.input[index0].range ? 
                [ desc.input[index0].range.min, desc.input[index0].range.max ] 
                    : [ "0", "0" ];
            const ybox: VarRange = desc.input?.length > index1 && desc.input[index1].range ?
                [ desc.input[index1].range.min, desc.input[index1].range.max ]
                    : [ "0", "0" ];
            const axes: { name: string, min: string, max: string }[] = [
                { name: desc.paving.variables[index0], ...desc.input[index0].range },
                { name: desc.paving.variables[index1], ...desc.input[index1].range }
            ];
            const backgroun_mesh: Mesh3DPlot = makeMesh3D([ xbox, ybox ], height, "gray");
    
            // background mesh needs to go first, otherwise it may occlude other boxes
            const data: Mesh3DPlot[] = [ backgroun_mesh ].concat(plotData);
            const message: Plot3DRequest = {
                command: JSCommand.plot3D, 
                data,
                axes
                // xaxis: desc.paving.variables[0],
                // yaxis: desc.paving.variables[1]
            };
            this.pane?.webview?.postMessage(message);
            return data;
        }
        return null;
    }
    /**
     * Plot parallel coordinates paving
     */
    plotParallelPaving (desc: PavingAnalysisResponse, opt?: { tickvals?: boolean, fullrange?: boolean }): void {
        if (desc?.paving) {
            const possibly: PavingCorners[] = desc?.paving?.possibly || [];
            const certainly: PavingCorners[] = desc?.paving?.certainly || [];
            const plotData: ParallelPlotData = makeParallelPlot({
                corners: possibly.concat(certainly), 
                variables: desc?.paving?.variables,
                input: desc?.input
            }, opt);
            const message: PlotParallelRequest = {
                command: JSCommand.parallelPlot, 
                data: [ plotData ],
                maxticks: plotData.maxticks,
                tickvals: !!opt?.tickvals
            };
            this.pane?.webview?.postMessage(message);
        }
    }
    /**
     * Show given data in a table
     */
    createTable (desc: {
        col1: { [xLabel: string]: BarPlotData }, 
        col2?: { [xLabel: string]: BarPlotData },
        col3?: { [xLabel: string]: BarPlotData }
        header?: string[]
    }, opt?: {
        labels?: { [xLabel: string]: string }
    }): void {
        if (desc?.col1) {
            const htmlTable: string = makeHtmlTable(desc, opt);
            const message: TableRequest = {
                command: JSCommand.table, 
                data: htmlTable
            };
            this.pane?.webview?.postMessage(message);
        }
    }
    /**
     * Show given data in a table
     */
    createTableMulti (desc: {
        paving: PavingData,
        header?: string[]
    }): void {
        if (desc?.paving) {
            const htmlTable: string = makeHtmlTableMulti(desc);
            const message: TableRequest = {
                command: JSCommand.table, 
                data: htmlTable
            };
            this.pane?.webview?.postMessage(message);
        }
    }
}