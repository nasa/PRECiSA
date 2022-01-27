/**
 * @module PvsLanguageClient
 * @author Paolo Masci
 * @date 2019.11.06
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
import * as path from 'path';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, TransportKind, ServerOptions, CancellationToken } from 'vscode-languageclient';
import * as vscodeUtils from './utils/vscode-utils';
import { PrecisaServerCommands, StartPrecisaRequest } from './common/precisa';
import { ViewsManager } from './views/viewsManager';
import { DEFAULT_DEPTH, DEFAULT_PRECISION } from './common/precisaUtils';

const server_path: string = path.join('server', 'out', 'vscodePrecisaServer.js');

export class PrecisaClient {
	// language client
	protected client: LanguageClient;

	// paths to executables and libs
	protected precisaPath: string;
	protected kodiakPath: string;

	// precisa options
	protected precision: number = DEFAULT_PRECISION;
	protected depth: number = DEFAULT_DEPTH;

	// context variables
	protected context: vscode.ExtensionContext;

	// webviews
	protected toolkitView: ViewsManager;

	// timers, for periodic events such as autosave
	protected timers: {[key: string]: NodeJS.Timer } = {};

	/**
	 * Internal function, returns the pvs path indicated in the configuration file
	 */
	protected getPrecisaPath (): string {
		return vscodeUtils.getConfiguration("precisa.path");
	}
	protected getKodiakPath (): string {
		return vscodeUtils.getConfiguration("precisa.xlib.kodiak");
	}

	/**
	 * Client activation function.
	 * @param context 
	 */
	async activate (context: vscode.ExtensionContext): Promise<void> {
		// save pointer to extension context
		this.context = context;
		
		// The server is implemented in NodeJS
		const serverModule = context.asAbsolutePath(server_path);
		// If the extension is launched in debug mode then the debug server options are used
		// Otherwise the run options are used
		const serverOptions: ServerOptions = {
			run: { module: serverModule, transport: TransportKind.ipc },
			debug: {
				module: serverModule,
				transport: TransportKind.ipc,
				options: { execArgv: ['--nolazy', '--inspect=9009'] } // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
			}
		};
		// Options to control the language client
		const clientOptions: LanguageClientOptions = {
			// Register the server for pvs files
			documentSelector: [{ scheme: 'file' }],
			synchronize: {
				// Notify the server about file changes to '.clientrc files contained in the workspace
				fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
			}
		};
		// Create the language client and start the client.
		this.client = new LanguageClient(
			'vscode-precisa',
			'vscode-precisa',
			serverOptions,
			clientOptions
		);
		
		// start client, which in turn will also start the server
		this.client.start(); 
		this.client.onReady().then(() => {
			// activate views
			this.toolkitView = new ViewsManager(this.client);
			this.toolkitView.activate(context);

			// register error handlers
			this.client.onNotification(PrecisaServerCommands.failure, (failure: { msg: string }) => {
				if (failure) {
					vscode.workspace.openTextDocument({ content: failure.msg }).then((document: vscode.TextDocument) => {
						vscode.window.showTextDocument(document.uri, { preserveFocus: true, preview: true, viewColumn: vscode.ViewColumn.Beside });
					});
				}
			});
			this.client.onNotification(PrecisaServerCommands.analysisError, (error: { msg: string }) => {
				if (error?.msg) {
					vscodeUtils.showWarningMessage(error.msg);
				} else {
					console.warn("[precisa-client] Warning: received analysi-error notification with null message");
				}
			});

			// restart precisa server when the configuration changes
			vscode.workspace.onDidChangeConfiguration((event: vscode.ConfigurationChangeEvent) => {
				// re-initialise pvs if the executable is different
				const precisaPath: string = vscodeUtils.getConfiguration("precisa.path");
				const kodiakPath: string = vscodeUtils.getConfiguration("precisa.xlib.kodiak");
				if (this.precisaPath !== precisaPath || this.kodiakPath !== kodiakPath) {
					this.precisaPath = precisaPath;
					this.kodiakPath = kodiakPath;
					// print debugging info
					let msg: string = `Restarting PRECiSA from ${this.precisaPath}`;
					if (this.kodiakPath) { msg += `\nKodiak: ${this.kodiakPath}`; }
					console.log(msg);
					// start server
					const req: StartPrecisaRequest = {
						precisaPath: this.precisaPath,
						kodiakPath: this.kodiakPath
					};
					this.client.sendRequest(PrecisaServerCommands.startServer, req);
				}
			});

			// start precisa server
			this.precisaPath = this.getPrecisaPath();
			this.kodiakPath = this.getKodiakPath();
			const req: StartPrecisaRequest = {
				precisaPath: this.precisaPath,
				kodiakPath: this.kodiakPath
			};
			this.client.sendRequest(PrecisaServerCommands.startServer, req);
		});
	}

	/**
	 * Stops the client
	 */
	async stop (): Promise<void> {
		if (this.client) {
			this.client.sendRequest(PrecisaServerCommands.stopServer);
			await this.client.stop();
		}
	}
}

// client instance
const precisaClient = new PrecisaClient();

/**
 * Function activate is invoked every time a new IDE session is started. This includes:
 *  - Opening a new folder in the IDE browser
 *  - ...
 * @param context 
 */
export function activate(context: vscode.ExtensionContext) {
	// Activate the client.
	precisaClient.activate(context); // async call
}

export function deactivate(): Thenable<void> {
	return new Promise(async (resolve, reject) => {
		await precisaClient.stop();
		resolve();
	});
}
