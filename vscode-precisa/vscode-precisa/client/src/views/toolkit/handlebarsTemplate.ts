/**
 * @module DomTemplates
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

/**
 * Commands accepted by the script embedded in the webview
 */
export enum JSCommand {
    parallelPlot = "parallel-plot",
    plot3D = "plot3D",
    barPlot = "bar-plot",
    table = "table",
    compareData = "compare-data",
    switchTo = "switch-to"
};

/**
 * Divs available in the html
 */
 export enum JSDiv {
    plotUnstable = "plot-unstable",
    plotErrorBounds = "plot-error-bounds",
    plotIntervals = "plot-intervals",
    plotSensitivity = "plot-sensitivity",
    plotComparative = "plot-comparative",
    plotPaving = "plot-paving"
};

import { Uri } from "vscode";
import { DEFAULT_DEPTH, DEFAULT_PRECISION } from "../../common/precisaUtils";

/**
 * Handlebars template for the html content
 */
export declare interface HtmlTemplate {
    title: string,
    body: string,
    style?: string,
    css?: Uri[],
    js?: Uri[]
};

export const rangeIn: string = "axis range is";
export const plotHeight: number = 300; //px
export const tickHeight: number = 12; //px
/**
 * Plot function and event handlers executed inside the webview
 * TODO: check if this function can be defined in a separate file and then imported as a module
 */
export const plotFunction: string = `
<style>
.legend {
    display: none;
}
.gtitle {
    font-size: small !important;
}
.plot {
    width: 100%;
    height: 0px;
    position: relative;
    z-index: 0;
}
.nav-link.active {
    font-weight:bold;
}
.checkmark__circle {
    stroke-dasharray: 166;
    stroke-dashoffset: 166;
    stroke-width: 2;
    stroke-miterlimit: 10;
    stroke: #7ac142;
    fill: none;
    animation: stroke 0.6s cubic-bezier(0.65, 0, 0.45, 1) forwards;
}
.checkmark {
    width: 56px;
    height: 56px;
    border-radius: 50%;
    display: block;
    stroke-width: 2;
    stroke: #fff;
    stroke-miterlimit: 10;
    margin: 10% auto;
    box-shadow: inset 0px 0px 0px #7ac142;
    animation: fill .4s ease-in-out .4s forwards, scale .3s ease-in-out .9s both;
}
.checkmark__check {
    transform-origin: 50% 50%;
    stroke-dasharray: 48;
    stroke-dashoffset: 48;
    animation: stroke 0.3s cubic-bezier(0.65, 0, 0.45, 1) 0.8s forwards;
}
@keyframes stroke {
    100% {
      stroke-dashoffset: 0;
    }
}
@keyframes scale {
    0%, 100% {
      transform: none;
    }
    50% {
      transform: scale3d(1.1, 1.1, 1);
    }
}
@keyframes fill {
    100% {
      box-shadow: inset 0px 0px 0px 30px #7ac142;
    }
}
</style>
<script>
// analysis mode
// enum Analysis { options, error-bounds, sensitivity, intervals, comparative, paving };
let mode = "error-bounds";

// flag indicating whether any analysis result is ready -- used for hiding/revealing the save button
let results_ready = false;

// compare with function
let compare_ready = false;

// plot height
let height = ${plotHeight};

// parallel plot data
let pdata = null;
let timer = null;

// plot options
const options = {
    modeBarButtonsToRemove: [ 
        'toImage', 'lasso2d', 'zoom2d', 'select2d', 
        'hoverClosestCartesian', 'hoverCompareCartesian', 'resetScale2d'
    ],
    displayModeBar: false,
    editable: false,
    scrollZoom: false,
    responsive: true,
    showlegend: false
};

// restyle parallel plot
function restyle (id) {
    if (pdata && mode === "paving") {
        const divName = "plot" + "-" + mode;
        const divElem = document.getElementById(divName);
        console.log("restyle", pdata, divName);
        // delayed refresh
        clearTimeout(timer);
        timer = setTimeout(() => {
            if (pdata && pdata.length && pdata[0].dimensions && pdata[0].dimensions.length) {
                // find dimension with constraintrange
                let rr = 0;
                for (let i = 1; i < pdata[0].dimensions.length; i++) {
                    if (pdata[0].dimensions[i].constraintrange && pdata[0].dimensions[i].constraintrange.length === 2) {
                        rr = i;
                        break;
                    }
                }
                if (pdata[0].dimensions[rr].constraintrange && pdata[0].dimensions[rr].constraintrange.length === 2) {
                    pdata[0].dimensions[rr].constraintrange[0] = +id - 0.5;
                    pdata[0].dimensions[rr].constraintrange[1] = +id + 0.5;
                    Plotly.newPlot(divElem, pdata, { height: ${2 * plotHeight} }, options); // restyle does not seem to do anything
                    // disable first axis of parallel plot so behavior of highlight is controlled by range slider
                    if ($(".background")[0]) { $(".background")[0].style.pointerEvents = "none"; }
                    // show tooltip
                    show_tooltips();
                }
            }
        }, 250);
    }
}

// show bootstrap tooltips
function show_tooltips () {
    // remove old tooltips
    $(".tooltip").remove();
    // generate new ones
    $(".axis-title")
        .attr("data-bs-toggle", "tooltip")
        .attr("data-bs-placement", "top")
        .attr("title", (i) => {
            const cond = !!pdata && !!pdata.length && !!pdata[0].dimensions && !!pdata[0].dimensions[0].values && !!pdata[0].dimensions[0].values.length;
            console.log({ i, pdata, cond });
            if (cond) {
                const index = +$('#rangeval').html()
                const min = pdata[0].dimensions[i].values[index * 2];
                const max = pdata[0].dimensions[i].values[index * 2 + 1];
                const label = pdata[0].dimensions[i].label ? pdata[0].dimensions[i].label.split(" ${rangeIn} [")[0] : "";
                console.log({ min, max, idxmin: index * 2, idxmax: index * 2 + 1, len: pdata[0].dimensions[i].values.length });
                return i === 0 ?
                    label + min 
                        : label + " in [" + min + ", " + max + "]";
            }
            return "";
        })
        .tooltip({ container: "#plot-paving", trigger: "manual" })
        .tooltip('show');
}

// plot function
function plot (plotData, layout, opt) {
    opt = opt || {};
    // adjust plot height
    height = (opt.tickvals && opt.maxticks * ${tickHeight} > ${plotHeight}) ?
        opt.maxticks * ${tickHeight} 
            : mode === "paving" ? ${plotHeight} : ${plotHeight};
    // show plot div
    show_plot();
    // create plot
    const divName = opt.div || "plot" + "-" + mode;
    const divElem = document.getElementById(divName);
    console.log("layout", layout);
    console.log("opt", opt);
    Plotly.newPlot(divElem, plotData, layout, options);
    if (mode === "paving") {
        // disable first axis of parallel plot so behavior of highlight is controlled by range slider
        if ($(".background")[0]) { $(".background")[0].style.pointerEvents = "none"; }
        // show tooltips
        show_tooltips();
    }
}
// show plot based on the current mode
function show_plot (opt) {
    opt = opt || {};
    // hide all plots
    $(".plot").css({ height: "0px", overflow: "hidden" });
    // reveal based on the tab
    if (mode === "options") {
        $("#analysis-pane").css({ opacity: 0 });
    } else {
        $("#analysis-pane").css({ opacity: 1 });
        // reveal plots based on the selected tab
        let divName = "plot" + "-" + mode;
        const h = mode === "paving" ? 2 * height + "px" : height + "px";
        console.log("show_plot " + divName + ", height " + h);
        $("#" + divName).css({ height: h, overflow: "visible" });
    }
}

// show paving ok
function show_paving_ok (divName) {
    $("#" + divName).html(''
    + '<figure class="text-center">'
    + '<blockquote class="blockquote">'
    + '<p>Paving results indicate all Ok!</p>'
    + '</blockquote>'
    + '<figcaption class="blockquote-footer">'
    + 'This means that the function does not contain unstable guards'
    + '</figcaption>'
    + '</figure>'
    + '<svg class="checkmark" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 52 52">'
    + '<circle class="checkmark__circle" cx="26" cy="26" r="25" fill="none"/>'
    + '<path class="checkmark__check" fill="none" d="M14.1 27.2l7.1 7.2 16.7-16.8"/>'
    + '</svg>');
}

// create table
function make_table (tableData) {
    const divName = "table" + "-" + mode;
    $("#" + divName).html(tableData);
}
// show table based on the current mode
function show_table () {
    // hide all tables
    $(".plot-table").css({ display: "none" });
    // reveal based on the tab
    if (mode === "options") {
        $("#analysis-pane").css({ opacity: 0 });
    } else {
        $("#analysis-pane").css({ opacity: 1 });
        // reveal tables based on the selected tab
        const tableName = "table" + "-" + mode;
        $("#" + tableName).css({ display: "block" });
    }
}

// switch to a given panel
function switch_to (md) {
    console.log("[plot-webview] Switching to " + md);
    switch (md) {
        case "options": {
            $("#pane0-tab").click();
            break;
        }
        case "error-bounds": {
            $("#pane1-tab").click();
            break;
        }
        case "sensitivity": {
            $("#pane2-tab").click();
            break;
        }
        case "intervals": {
            $("#pane3-tab").click();
            break;
        }
        case "comparative": {
            $("#pane4-tab").click();
            break;
        }
        case "paving": {
            $("#pane5-tab").click();
            break;
        }
        default: {
            console.log("[plot-webview] Warning: trying to switch to unknown mode", md);
            break;
        }
    }
}

// render webview buttons
function show_buttons () {
    // render save button
    $("#save").css("display", results_ready && mode === "error-bounds" ? "inline-block" : "none");
    // render analyze button
    (mode === "comparative" && !compare_ready) ? 
        $("#analyze").prop("disabled", true).addClass("btn-outline-secondary").removeClass("btn-outline-primary")
            : $("#analyze").prop("disabled", false).addClass("btn-outline-primary").removeClass("btn-outline-secondary");
    (mode === "comparative") ?
        $("#analyze").html("Compare")
            : $("#analyze").html("Analyze");
    // render slide range for paving
    $(".parallel-plot").css("display", (mode === "paving" && results_ready && !!pdata) ? "block" : "none");
}

// renders buttons, plots, tables
function refresh_view () {
    show_buttons();
    show_plot();
    show_table();
}

// utility functions to send messages from the webview to vscode
(function() {
    // vscode API
    const vscode = acquireVsCodeApi();

    // change current mode
    function current_mode (md) {
        mode = md;
        vscode.postMessage({
            command: "change-mode",
            mode
        });
        (mode === "paving") ? $(".tooltip").show() : $(".tooltip").hide();
    };

    // handlers for html elements
    $("#pane0-tab").on("click", () => { current_mode("options"); refresh_view(); });
    $("#pane1-tab").on("click", () => { current_mode("error-bounds"); refresh_view(); });
    $("#pane2-tab").on("click", () => { current_mode("sensitivity"); refresh_view(); });
    $("#pane3-tab").on("click", () => { current_mode("intervals"); refresh_view(); });
    $("#pane4-tab").on("click", () => { current_mode("comparative"); refresh_view(); });
    $("#pane5-tab").on("click", () => { current_mode("paving"); refresh_view(); });
    $("#analyze").on("click", (evt) => {
        console.log("collecting info...");
        let inputData = []; // { name: string, range: { min: string, max: string }}[];
        let varnames = $(".varname");
        for (let i = 0; i < varnames.length; i++) {
            const name = varnames[i].textContent;
            const min = $("#" + name + "-min")[0].value;
            const max = $("#" + name + "-max")[0].value;
            let range = { min, max };
            if (mode === "sensitivity") { range.uncertainty = $("#uncertainty")[0].value; };
            if (mode === "intervals") { range.intervals = $("#intervals")[0].value; };
            inputData.push({ name, range });
        }
        const precision = parseInt($("#option-precision")[0].value);
        const depth = parseInt($("#option-depth")[0].value);
        const selected = mode === "paving" && $(".plot3D-var option:selected").length > 1 ?
            [ $(".plot3D-var option:selected")[0].text, $(".plot3D-var option:selected")[1].text ] 
                : undefined;
        let options = {
            precision: precision >= 1 ? precision : 1,
            depth: depth >= 1 ? depth : 1,
            selected
        };
        console.log({ inputData, options });
        console.log("starting analysis...");
        vscode.postMessage({
            command: "compute-" + mode,
            data: inputData,
            options
        });
    });
    $("#save").on("click", (evt) => {
        vscode.postMessage({
            command: "save-results"
        });
    });
}());

// Handlers for message received from vscode
window.addEventListener('message', event => {
    const message = event.data; // JSON data sent by vscode-pvs
    if (message) {
        const node = document.getElementById(message.id);
        switch (message.command) {
            case "bar-plot": {
                console.log("[precisa-webview] bar-plot", message.data); // message.data is of type PlotData[]
                // create layout
                const layout = {
                    xaxis: {
                        title: { text: message.xaxis || ' ' },
                        tickangle: -45,
                        showticklabels: !!message.showxlabels,
                        fixedrange: !!message.xrangefixed
                    },
                    yaxis: {
                        title: { text: message.yaxis || '  Error  ' },
                        zeroline: false,
                        gridwidth: 2,
                        type: "log",
                        autorange: true,
                        constraintoward: "bottom",
                        // dtick: 1,
                        automargin: true,
                        fixedrange: !!message.yrangefixed,
                        tickformat: ".8e~" // see https://github.com/d3/d3-format/blob/main/README.md#locale_format
                    },
                    barmode: "overlay",
                    bargap: 0.05,
                    title: message.title,
                    font: {
                        size: 10
                    },
                    grid: { 
                        rows: message.rows || 1, 
                        columns: message.cols || 1, 
                        pattern: 'independent' 
                    }
                };
                const opt = {
                    div: message.div
                };
                const plotData = message.data;
                console.log("[precisa-webview] plot args", { plotData, layout, opt });
                plot(plotData, layout, opt);
                // collapse mesh plot
                $("#mesh-plot").css("height", "0px");
                // show save button
                results_ready = true;
                refresh_view();
                break;
            }
            case "plot3D": {
                console.log("[precisa-webview] plot3D", message); // message is of type Plot3DRequest
                const div = "mesh-plot";
                // set div height
                $("#mesh-plot").css("height", ${2 * plotHeight} + "px");
                // pull table down
                // $("#table-paving").css("margin-top", ${2 * plotHeight} + "px");
                const plotData = message.data;
                const xaxis = message.axes && message.axes.length > 0 ? message.axes[0].name : "";
                const yaxis = message.axes && message.axes.length > 1 ? message.axes[1].name : "";
                const layout = {
                    scene: {
                        xaxis:{ title: xaxis || ' ' },
                        yaxis:{ title: yaxis || ' ' },
                        zaxis:{
                            title: ' ',
                            showticklabels: false
                        }
                    },
                    height: ${2 * plotHeight},
                    title: message.title || ' ',
                    font: {
                        size: 10
                    },
                    plot_bgcolor: "transparent",
                    paper_bgcolor: "transparent",
                    margin: {
                        t: 0
                    }
                };
                $("#mesh-plot").empty();
                plotData?.length > 1 ? plot(plotData, layout, { div }) : show_paving_ok("mesh-plot");
                // show save button
                results_ready = true;
                refresh_view();
                break;
            }
            case "parallel-plot": {
                console.log("[precisa-webview] parallel-plot", message.data); // message.data is of type PlotData[]
                const div = "parallel-plot";
                const plotData = message.data;
                const maxticks = +message.maxticks - 1;
                const tickvals = !!message.tickvals;
                const selected = parseInt(maxticks / 2);
                const layout = {
                    title: message.title || ' ',
                    height: ${2 * plotHeight}
                };
                $("#formControlRange").attr("max", maxticks).attr("value", selected);
                $("#rangeval").html(selected);
                // save data for restyle
                pdata = plotData;
                (maxticks > 0) ? 
                    plot(plotData, layout, { maxticks, tickvals, div })
                        : show_paving_ok("parallel-plot");
                // show save button
                results_ready = true;
                refresh_view();
                break;
            }
            case "table": {
                console.log("[precisa-webview] table", message.data); // message.data is of type TableData[]
                const tableData = message.data;
                make_table(tableData);
                refresh_view();
                break;
            }
            case "compare-data": {
                if (message.fun) {
                    console.log("[precisa-webview] compare-data", message.data); // message.data is of type PlotData[]
                    // compile the input field
                    compare_ready = !!message?.fun;
                    $("#compare-with").val(message?.fun);
                    // switch to comparative analysis view
                    switch_to("comparative");
                }
                break;
            }
            case "switch-to": {
                switch_to(message.mode);
                break;
            }
            default: {
                break;
            }
        }
    }
});
// render buttons, plots, tables
refresh_view();
</script>
`;


/**
 * Handlebar Templates
 */
export const htmlTemplate: string = `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{title}}</title>
    {{#if style}}
    <style type="text/css">{{style}}</style>
    {{/if}}
    {{#each css}}
    <link rel="stylesheet" href="{{this}}">
    {{/each}}
    <style>
    .card-header {
        max-height: 52px !important;
        overflow: auto;
        font-size: small;
    }
    .card-body {
        font-size: small;
    }
    .input-group-text {
        font-size: small;
    }
    </style>
    {{#each js}}
    <script src="{{this}}"></script>
    {{/each}}
</head>
<body style="margin:0; padding:0; width:105%; transform:scale(0.95); transform-origin:top left;">
    <div class="card">
        <div class="card-header">
            {{title}}
        </div>
        <div class="card-body px-2 flex">
            {{body}}
        </div>
    </div>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    ${plotFunction}
</body>
</html>
`;

// minimum width of a column in the input table
export const MIN_WIDTH: number = 160;

// body template, includes: input table, analyze button, and plot
export const bodyTemplate: string = `
<!-- tabs -->
<nav>
  <div class="nav nav-tabs" id="nav-tab" role="tablist">
    <button class="nav-link fa fa-cogs" id="pane0-tab" data-bs-toggle="tab" data-bs-target="#pane0" type="button" role="tab" aria-controls="pane0" aria-selected="false"></button>
    <button class="nav-link active" id="pane1-tab" data-bs-toggle="tab" data-bs-target="#pane1" type="button" role="tab" aria-controls="pane1" aria-selected="true">Round-Off Error</button>
    <button class="nav-link" id="pane2-tab" data-bs-toggle="tab" data-bs-target="#pane2" type="button" role="tab" aria-controls="pane2" aria-selected="false">Sensitivity</button>
    <button class="nav-link" id="pane3-tab" data-bs-toggle="tab" data-bs-target="#pane3" type="button" role="tab" aria-controls="pane3" aria-selected="false">Intervals</button>
    <button class="nav-link" id="pane4-tab" data-bs-toggle="tab" data-bs-target="#pane4" type="button" role="tab" aria-controls="pane4" aria-selected="false">Comparison</button>
    <button class="nav-link" id="pane5-tab" data-bs-toggle="tab" data-bs-target="#pane5" type="button" role="tab" aria-controls="pane5" aria-selected="false">Paving</button>
  </div>
</nav>

<div class="shadow" style="border-left:1px solid lightgray; border-right:1px solid lightgray;">
    <div class="tab-content p-3" id="nav-tabContent">
        <div class="tab-pane" id="pane0" role="tabpanel" aria-labelledby="pane0-tab">
            <b>Analysis Options</b>: The numerical precision and the depth of the branch-and-bound analysis can be customized using the following two parameters.
            <div class="input-group input-group-sm mb-0 py-3">
                <div class="input-group-prepend">
                    <span class="input-group-text" style="width:${MIN_WIDTH}px;">Precision (default: ${DEFAULT_PRECISION})</span>
                </div>
                <input type="number" min="1" max="100" id="option-precision" class="form-control" value="${DEFAULT_PRECISION}">
            </div>
            <div class="input-group input-group-sm mb-3">
                <div class="input-group-prepend">
                    <span class="input-group-text" style="width:${MIN_WIDTH}px;">Depth (default: ${DEFAULT_DEPTH})</span>
                </div>
                <input type="number" min="1" max="30" id="option-depth" class="form-control" value="${DEFAULT_DEPTH}">
            </div>
        </div>
        <div class="tab-pane show active" id="pane1" role="tabpanel" aria-labelledby="pane1-tab">
            <b>Round-Off Error Estimation</b>: Computes a sound estimation of the accumulated floating-point round-off error that can occur in function {{functionName}} for given ranges of input variables. In addition, it soundly estimates the error associated to unstable guards in the function.
        </div>
        <div class="tab-pane" id="pane2" role="tabpanel" aria-labelledby="pane2-tab">
            <b>Sensitivity Analysis</b>: Evaluates the sensitivity of the floating-point round-off error when the range of input values is affected by a given uncertainty level.
            <div class="input-group input-group-sm py-3">
                <div class="input-group-prepend">
                    <span class="input-group-text" id="inputGroup-sizing-sm" style="min-width:${MIN_WIDTH}px;">Uncertainty Level</span>
                </div>
                <input type="text" class="form-control" aria-label="Small" aria-describedby="inputGroup-sizing-sm" value="1%" id="uncertainty">
            </div>
        </div>
        <div class="tab-pane" id="pane3" role="tabpanel" aria-labelledby="pane3-tab">
            <b>Interval Analysis</b>: Divides the input range into intervals and computes the floating-point round-off error for each interval.
            <div class="input-group input-group-sm py-3">
                <div class="input-group-prepend">
                    <span class="input-group-text" id="inputGroup-sizing-sm" style="min-width:${MIN_WIDTH}px;">Number of Intervals</span>
                </div>
                <input type="text" class="form-control" aria-label="Small" aria-describedby="inputGroup-sizing-sm" value="10" id="intervals">
            </div>
        </div>
        <div class="tab-pane" id="pane4" role="tabpanel" aria-labelledby="pane4-tab">
            <b>Comparative Analysis</b>: Compares the floating-point round-off error of two functions evaluated on the same input variables.
            <div class="input-group input-group-sm mb-3 py-3">
                <input disabled type="text" style="background:white;" class="form-control" value="{{functionName}}">
                <span class="input-group-text">vs.</span>
                <input disabled id="compare-with" type="text" style="background:white;" class="form-control" placeholder="Use 'compare-error-bounds' to select function">
            </div>
        </div>
        <div class="tab-pane" id="pane5" role="tabpanel" aria-labelledby="pane5-tab">
            <b>Paving</b>: Plots a mesh diagram highlighting the range of input variables that can trigger unstable guards.
            <div class="input-group input-group-sm mb-3 py-3">
                <!-- selection buttons for plot3d/mesh3d-->
                <div class="input-group">
                    <div class="input-group-prepend">
                        <span class="input-group-text" id="">Variables to be plotted</span>
                    </div>
                    <select id="plot3D-var1" class="plot3D-var form-select form-select-sm" aria-label="Default select example">
                        {{#each inputData}}
                        <option {{#if @first}}selected{{/if}} value="{{name}}">{{name}}</option>
                        {{/each}}
                    </select>
                    <select id="plot3D-var2" class="plot3D-var form-select form-select-sm" aria-label="Default select example">
                        {{#each inputData}}
                        <option {{#if @last}}selected{{/if}} value="{{name}}">{{name}}</option>
                        {{/each}}
                    </select>
                </div>
            </div>
        </div>
    </div>

    <div id="analysis-pane">
        <!-- input table -->
        <div class="px-3">
            <div class="input-group input-group-sm mb-0">
                <div class="input-group-prepend">
                    <span class="input-group-text bg-primary" id="{{name}}" style="width:${MIN_WIDTH}px; color:white;">INPUT</span>
                </div>
                <input type="text" class="form-control bg-primary" style="color:white;" value="MIN" disabled>
                <input type="text" class="form-control bg-primary" style="color:white;" value="MAX" disabled>
            </div>
            {{#each inputData}}
            <div class="input-group input-group-sm {{#if @last}}mb-3{{else}}mb-0{{/if}}">
                <div class="input-group-prepend">
                    <span class="input-group-text varname" id="{{name}}" style="width:${MIN_WIDTH}px;">{{name}}</span>
                </div>
                <input type="text" id="{{name}}-min" class="form-control" value="{{range.min}}">
                <input type="text" id="{{name}}-max" class="form-control" value="{{range.max}}">
            </div>
            {{/each}}
        </div>

        <!-- analyze button -->
        <div class="p-3" style="position:relative; z-index:1;">
            <button data-bs-toggle="tooltip" data-bs-placement="top" title="Tooltip on top" id="analyze" class="btn btn-outline-primary btn-sm" style="width:${MIN_WIDTH}px;">Analyze</button>
            <button id="save" class="btn btn-outline-success btn-sm" style="width:${MIN_WIDTH}px; display:none; float:right;">Save Results</button>
        </div>
        <!-- slide range for parallel plot -->
        <div class="btn btn-sm parallel-plot" style="display:none;">
            <label for="formControlRange" class="form-label">Box #<span id="rangeval">1</span></label>
            <input type="range" class="form-range" 
                id="formControlRange" 
                min="0" max="1" value="1" step="1" 
                onInput="$('#rangeval').html($(this).val()); restyle($(this).val());">
        </div>

        <!-- plot -->
        <div id="plot-sensitivity" class="plot"></div>
        <div id="plot-intervals" class="plot"></div>
        <div id="plot-comparative" class="plot"></div>

        <div id="plot-paving" class="plot">
            <div id="parallel-plot" class="subplot" style="display: none; height:${2 * plotHeight}px;"></div>
            <div id="mesh-plot" class="subplot"></div>
        </div>

        <div id="plot-error-bounds" class="plot"></div>
        <div id="plot-unstable" class="plot"></div>

        <!-- tables -->
        <div id="table-sensitivity" class="plot-table card p-3"></div>
        <div id="table-intervals" class="plot-table card p-3"></div>
        <div id="table-comparative" class="plot-table card p-3"></div>
        <div id="table-paving" class="plot-table card p-3"></div>
        <div id="table-error-bounds" class="plot-table card p-3"></div>
        <div id="table-unstable" class="plot-table card p-3"></div>

    </div>
</div>
`;

export interface TableTemplate {
    header?: string[], // header labels, one for each col
    rows: {          // table content, a series of rows, each made of a series of cols. First col is used as row label.
        cols: string[]
    }[]
};
export const tableTemplate: string = `
<table class="table table-hover table-sm">
  <thead>
    <tr>
    {{#each header}}
      <th scope="col">{{this}}</th>
    {{/each}}
    </tr>
  </thead>
  <tbody>
    {{#each rows}}
    <tr>
        {{#each cols}}
        {{#if @first}}<th scope="row">{{this}}</th>{{else}}<td style="white-space:nowrap;">{{this}}</td>{{/if}}
        {{/each}}
    </tr>
    {{/each}}
  </tbody>
</table>
`;