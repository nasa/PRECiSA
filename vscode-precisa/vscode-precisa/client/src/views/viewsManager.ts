/**
 * @module PrecisaAnalysisView
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

import { commands, ExtensionContext, window } from "vscode";
import { LanguageClient } from "vscode-languageclient";
import { FormulaDesc } from "../common/precisa";
import { 
    DidChangeViewStateEvent, DidDisposeEvent, Mode, ToolkitView, PlotViewEvents
} from "./toolkit/toolkitView";

/**
 * Views manager
 */
export class ViewsManager {
    // client for interacting with the server back-end
    protected client: LanguageClient;
    // client context
    protected context: ExtensionContext;
    // plot views
    // protected plotViews: PlotView[] = [];
    // active view
    protected activeView: ToolkitView;

    /**
     * Constructor
     */
    constructor (client: LanguageClient) {
		this.client = client;
    }
    /**
     * Activates the module
     */
    activate (context: ExtensionContext) {
        this.context = context;
        // codelens handlers
        context.subscriptions.push(commands.registerCommand("vscode-precisa.show-analysis-view", async (desc: FormulaDesc) => {
            if (desc) {
                // save file currently open in the editor
                if (window?.activeTextEditor?.document) {
                    await window.activeTextEditor.document.save();
                }
                // create a new analysis view
                this.createView(desc);
            }
        }));
        context.subscriptions.push(commands.registerCommand("vscode-precisa.show-split-view", async (desc: FormulaDesc) => {
            if (desc) {
                // save file currently open in the editor
                if (window?.activeTextEditor?.document) {
                    await window.activeTextEditor.document.save();
                }
                // compare with the given function (create a new analysis view if no view is active)
                if (this.activeView) {
                    await this.activeView.compareWith(desc);
                } else {
                    this.createView(desc);
                    this.activeView.switchTo(Mode.comparative);
                    this.activeView.focus();
                }
            }
        }));
    }
    /**
     * Clears all stored tables and diagrams
     */
    clearPlots (): void {
        this.activeView?.clearPlots();
    }
    /**
     * Internal function, handles backbone events
     */
    protected onDidChangeViewState (evt: DidChangeViewStateEvent): void {
        if (evt?.view && evt?.view !== this.activeView) {
            this.activeView = evt.view;
        }
    }
    /**
     * Internal function, handles backbone events
     */
    protected onDidDispose (evt: DidDisposeEvent): void {
        if (evt?.view === this.activeView) {
            this.activeView = null;
        }
    }
    /**
     * Main API, renders a new webview
     */
    async createView (desc: FormulaDesc): Promise<void> {
        // create and activate a new view
        this.activeView = new ToolkitView(this.client);
        this.activeView.activate(this.context);
        // save the created view in the pool of known plot views
        // this.plotViews.push(this.activeView);
        // listen to backbone events
        this.activeView.on(PlotViewEvents.onDidChangeViewState, (evt: DidChangeViewStateEvent) => {
            this.onDidChangeViewState(evt);
        });
        this.activeView.on(PlotViewEvents.onDidDispose, (evt: DidDisposeEvent) => {
            this.onDidDispose(evt);
        });
        // render the view
        await this.activeView.render(desc);
    }
}