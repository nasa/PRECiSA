/**
 * @module vscode-utils
 * @author Paolo Masci
 * @date 2019.06.18
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
import * as vscode from "vscode";
import { CancellationToken } from "vscode-languageclient";
export function log(str: string) { vscode.window.showInformationMessage(str); }
import * as fsUtils from '../common/fsUtils';

/**
 * Returns the context folder of the editor
 */
export function getEditorContextFolder () : string {
    return (vscode.window.activeTextEditor) ? fsUtils.getContextFolder(vscode.window.activeTextEditor.document.fileName) : null;
}

/**
 * Returns the value of a vscode configuration key
 */
export function getConfiguration (key: string): string {
    const config: vscode.WorkspaceConfiguration = vscode.workspace.getConfiguration();
    const res: string | Object = config.get(key);
    return (typeof res === "string") ? res.trim() : "";
}

/**
 * Returns the boolean value of a vscode configuration key
 */
export function getConfigurationFlag (key: string): boolean {
    const config: vscode.WorkspaceConfiguration = vscode.workspace.getConfiguration();
    const res: boolean | Object = config.get(key);
    return !!res;
}

/**
 * Returns the numeric value of a vscode configuration key
 */
export function getConfigurationValue (key: string): number {
    const config: vscode.WorkspaceConfiguration = vscode.workspace.getConfiguration();
    const res: number | Object = config.get(key);
    return typeof res === "number" ? res : 0;
}

/**
 * Utility function, shows a temporary dialog with an information message.
 * The default timeout for the dialog is 3.2 seconds.
 */
export function showInformationMessage (message: string, opt?: { timeout?: number, cancellable?: boolean }): void {
    if (message) {
        opt = opt || {};
        const timeout: number = opt.timeout || 3200;
        const cancellable: boolean = !!opt.cancellable;
        const task = (progress: vscode.Progress<{ message: string, increment?: number }>, token: CancellationToken): Promise<void> => {
            progress.report({ increment: 100, message });
            return new Promise((resolve, reject) => {
                setTimeout(() => {
                    resolve();
                }, timeout);
            });
        }
        vscode.window.withProgress({
            location: vscode.ProgressLocation.Notification,
            cancellable
        }, task);
    }
}
/**
 * Utility function, shows a dialog presenting a warning message.
 */
export function showWarningMessage (message: string, timeout?: number): void {
    vscode.window.showWarningMessage(`${icons.sparkles} ${message}`);
    // showInformationMessage(`${utils.icons.sparkles} ${message}`, { timeout: 4000 });
}
// useful icons
export const icons: {
	checkmark: string,
	bang: string,
	snowflake: string,
	sparkles: string,
	whitecircle: string
} = {
	checkmark: "✅",
	bang : "❗",
	snowflake : "❄️",
	sparkles: "✨",
	whitecircle: "⚪"
};


/**
 * css colors
 */
export const cssColors: string[] = [
    "aliceblue", "antiquewhite", "aqua", "aquamarine", "azure",
    "beige", "bisque", "black", "blanchedalmond", "blue",
    "blueviolet", "brown", "burlywood", "cadetblue",
    "chartreuse", "chocolate", "coral", "cornflowerblue",
    "cornsilk", "crimson", "cyan", "darkblue", "darkcyan",
    "darkgoldenrod", "darkgray", "darkgrey", "darkgreen",
    "darkkhaki", "darkmagenta", "darkolivegreen", "darkorange",
    "darkorchid", "darkred", "darksalmon", "darkseagreen",
    "darkslateblue", "darkslategray", "darkslategrey",
    "darkturquoise", "darkviolet", "deeppink", "deepskyblue",
    "dimgray", "dodgerblue", "firebrick",
    "floralwhite", "forestgreen", "fuchsia", "gainsboro",
    "ghostwhite", "gold", "goldenrod", "gray", "green",
    "greenyellow", "honeydew", "hotpink", "indianred", "indigo",
    "ivory", "khaki", "lavender", "lavenderblush", "lawngreen",
    "lemonchiffon", "lightblue", "lightcoral", "lightcyan",
    "lightgoldenrodyellow", "lightgray", "lightgrey",
    "lightgreen", "lightpink", "lightsalmon", "lightseagreen",
    "lightskyblue", "lightslategray", "lightslategrey",
    "lightsteelblue", "lightyellow", "lime", "limegreen",
    "linen", "magenta", "maroon", "mediumaquamarine",
    "mediumblue", "mediumorchid", "mediumpurple",
    "mediumseagreen", "mediumslateblue", "mediumspringgreen",
    "mediumturquoise", "mediumvioletred", "midnightblue",
    "mintcream", "mistyrose", "moccasin", "navy",
    "oldlace", "olive", "olivedrab", "orange", "orangered",
    "orchid", "palegoldenrod", "palegreen", "paleturquoise",
    "palevioletred", "papayawhip", "peachpuff", "peru", "pink",
    "plum", "powderblue", "purple", "red", "rosybrown",
    "royalblue", "saddlebrown", "salmon", "sandybrown",
    "seagreen", "seashell", "sienna", "silver", "skyblue",
    "slateblue", "slategray", "slategrey", "snow", "springgreen",
    "steelblue", "tan", "teal", "thistle", "tomato", "turquoise",
    "violet", "wheat", "white", "whitesmoke", "yellow",
    "yellowgreen"
];