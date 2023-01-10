/**
 * @module PrecisaServer
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

import { 
	Connection, TextDocuments, TextDocument, createConnection, ProposedFeatures, InitializeParams, 
	CodeLens, CodeLensParams, Diagnostic, DiagnosticSeverity, TextDocumentChangeEvent, 
	TextDocumentSyncKind, DiagnosticRelatedInformation, MarkupContent
} from 'vscode-languageserver';
import * as fsUtils from './common/fsUtils';
import * as path from 'path';
import { 
	ComparativeAnalysisResponse,
	IntervalAnalysisResponse, PavingAnalysisResponse, PrecisaAnalysisRequest, PrecisaAnalysisResponse, PrecisaComparativeAnalysisRequest, PrecisaInputData, 
	PrecisaSaveResultsRequest, PrecisaSaveResultsResponse, PrecisaServerCommands, SensitivityAnalysisResponse, StartPrecisaRequest
} from './common/precisa';
import { PrecisaCodeLensProvider } from './providers/precisaCodeLensProvider';
import { DEFAULT_DEPTH, DEFAULT_PRECISION, formatMessage, precisaInputDataToString } from './common/precisaUtils';
import { paving2Json, PavingData } from './common/paverUtils';
import { execPrecisa } from './processWorker';

/**
 * This is part of the Diagnostic interface
 */
export interface Settings {
	maxNumberOfProblems: number;
};

/**
 * Diagnostic info
 */
 export declare interface PrecisaDiagnostic extends Diagnostic {
    unstableErrorBound?: string,
    errorBound?: string
};

/**
 * Utility function, clonse precisa input data
 */
export function clone(input: PrecisaInputData[], opt?: { group?: string }): PrecisaInputData[] {
	if (input) {
		opt = opt || {};
		let clone: PrecisaInputData[] = [];
		for (let k = 0; k < input.length; k++) {
			clone.push({
				name: input[k].name,
				range: {
					min: input[k].range.min,
					max: input[k].range.max
				},
				group: opt.group || undefined
			});
		}
		return clone;
	}
	return null;
}

export class VSCodePrecisaServer {
	// path to precisa folder
	protected precisaPath: string;
  	// path to kodiak
	protected kodiakPath: string;
	// connection to the client
	protected connection: Connection;
	// list of documents opened in the editor
	protected documents: TextDocuments;
	// client capabilities
	protected clientCapabilities: {
		hasConfigurationCapability: boolean,
		hasWorkspaceFolderCapability: boolean,
		hasDiagnosticRelatedInformationCapability: boolean
	}
	// client settings
	protected settings: {
		global: Settings,
		documents: Map<string, Thenable<Settings>>
	}

	/**
	 * Service providers
	 */
	protected codeLensProvider: PrecisaCodeLensProvider;

	/**
	 * Data structures used for performance improvements
	 */
	protected lastParsedContext: string; // this is used to avoid re-parsing a context

	/**
	 * @constructor
	 */
	constructor () {
		this.settings = {
			global: { maxNumberOfProblems: 1000 },
			documents: new Map()
        };
		this.clientCapabilities = {
			hasConfigurationCapability: false,
			hasWorkspaceFolderCapability: false,
			hasDiagnosticRelatedInformationCapability: false
		};
		try {
			// Create a connection channel to allow clients to connect.
			// The connection uses Node's IPC as a transport. Includes all proposed LSP features.
			this.connection = createConnection(ProposedFeatures.all);
			this.setupConnectionManager();

			// Create a simple text document manager. The text document manager supports full document sync only
			this.setupDocumentsManager(this.connection);

			// Listen on the connection
			this.connection.listen();
		} catch (connectionError) {
			console.warn(`[precisa-server] Warning: Unable to create LSP connection with client front-end`);
		} 
	}

	//------------------------------------------------------------------------------------------
	//                          INTERNAL FUNCTIONS
	//------------------------------------------------------------------------------------------

	/**
	 * Internal function, used by restartPvs
	 */
	protected createServiceProviders(): void {
		this.codeLensProvider = new PrecisaCodeLensProvider(this);
	}

	/**
	 * Internal function, install document managers
	 */
	protected setupDocumentsManager (connection: Connection) {
		this.documents = new TextDocuments();

		// -- Close the file only if it has been deleted from the context?
		this.documents.onDidClose((close: TextDocumentChangeEvent) => {
			if (close?.document?.uri) {
				// remove file descriptor
				this.settings.documents.delete(close.document.uri);
			}
		});

		// onDidSave fires when a document is saved on the editor
		this.documents.onDidSave(async (save: TextDocumentChangeEvent) => {
		});

		// Listen to document events triggered by the editor
		this.documents.listen(connection);
	}


	/**
	 * Internal function, used to setup LSP event listeners
	 */
	protected setupConnectionManager () {
        // declare server capabilities
		this.connection?.onInitialize((params: InitializeParams) => {
			// console.log(`--------- Client capabilities ---------\n`, params.capabilities);
			const capabilities = params.capabilities;
			this.clientCapabilities = {
				hasConfigurationCapability: capabilities.workspace && !!capabilities.workspace.configuration,
				hasWorkspaceFolderCapability: capabilities.workspace && !!capabilities.workspace.workspaceFolders,
				hasDiagnosticRelatedInformationCapability: capabilities.textDocument && capabilities.textDocument.publishDiagnostics && capabilities.textDocument.publishDiagnostics.relatedInformation
			}
			return {
				capabilities: {
					textDocumentSync: TextDocumentSyncKind.Full,
					// CodeLens provider returns commands that can be shown inline along with the specification
					codeLensProvider: {
						resolveProvider: true
					}
				}
			};
		});
        // initialize request handlers
		this.connection?.onInitialized(async () => {
			// install handlers
			this.connection.onRequest(PrecisaServerCommands.startServer, async (req: StartPrecisaRequest) => {
				this.precisaPath = fsUtils.tildeExpansion(req?.precisaPath);
                this.kodiakPath = fsUtils.tildeExpansion(req?.kodiakPath);
				// create service providers
				this.createServiceProviders();
			});
			// handlers for client requests
			this.connection.onRequest(PrecisaServerCommands.computePaving, async (req: PrecisaAnalysisRequest) => {
				this.pavingAnalysis(req); // async call
			});
			this.connection.onRequest(PrecisaServerCommands.computeErrorBounds, async (req: PrecisaAnalysisRequest) => {
				this.errorBoundsAnalysis(req); // async call
			});
			this.connection.onRequest(PrecisaServerCommands.computeSensitivity, async (req: PrecisaAnalysisRequest) => {
				this.sensitivityAnalysis(req); // async call
			});
			this.connection.onRequest(PrecisaServerCommands.computeIntervals, async (req: PrecisaAnalysisRequest) => {
				this.intervalAnalysis(req); // async call
			});
			this.connection.onRequest(PrecisaServerCommands.computeComparative, async (req: PrecisaComparativeAnalysisRequest) => {
				this.comparativeAnalysis(req); // async call
			});
			this.connection.onRequest(PrecisaServerCommands.saveResults, async (req: PrecisaSaveResultsRequest) => {
				this.saveResults(req); // async call
			});
		});
		// codelens
		this.connection?.onCodeLens(async (args: CodeLensParams): Promise<CodeLens[]> => {
			if (this.codeLensProvider) {
				const uri: string = args?.textDocument?.uri;
				if (fsUtils.isPvsFile(uri) && this.codeLensProvider) {
					const document: TextDocument = this.documents?.get(args?.textDocument.uri);
					const txt: string = document?.getText() || await this.readFile(uri);
					const codelens: CodeLens[] = await this.codeLensProvider.provideCodeLens({ txt, uri });
					return codelens;
				}
			}
			return null;
		});	
	}

	/**
	 * Internal function, reads the content of fileName from disk
	 */
	protected async readFile (fileName: string): Promise<string> {
		if (fileName) {
			fileName = decodeURIComponent(fileName);
			fileName = fileName.startsWith("file://") ? fileName = fileName.replace("file://", "") : fileName;
			const doc: TextDocument = this.documents.get("file://" + fileName);
			if (doc) {
				return doc.getText();
			}
			try {
				return fsUtils.readFile(fileName);
			} catch (readError) {
				console.error(`[precisa-server] Warning: Error while reading file ${fileName} (${readError.message})`);
				this.connection?.sendNotification(PrecisaServerCommands.failure, `Error while reading file ${fileName} (${readError.message})`);
			}
		} else {
			console.warn("[precisa-server] Warning: trying to read null filename");
		}
		return null;
	}


	//--------------------------------------------------------------------
	//                                APIs
	//--------------------------------------------------------------------

    /**
     * Computes regions with paver
     */
	protected async computePaving (req: PrecisaAnalysisRequest): Promise<PavingData> {
        if (req) {
            const msg: string = await execPrecisa(req, { precisaPath: this.precisaPath, kodiakPath: this.kodiakPath }, { paving: true, connection: this.connection });
			if (msg) {
				// process results
				// group 1 is the paving file name
				let match: RegExpMatchArray = /\bPaving\s*for\s*function\s*[^\s]+\s*generated\s*in\s*:\s*(.*)/g.exec(msg);
				const paving_file: string = (match && match.length > 1) ? match[1] : null;
				if (paving_file) {
					const paving: string = await fsUtils.readFile(paving_file);
					const pavingData: PavingData = paving2Json(paving);
					return pavingData;
				}
			}
        }
		return null;

		// _num_cert.pvs\nPaving for function e1 generated in: /var/folders/n4/q42bj8nd5dd80q05k8s1gv0w0000gq/T/1637789554648/example.e1.paving\n

		// const fname: string = path.join(os.tmpdir(), `${data.fileName}.dat`); // file generated by precisa
		// const ans: string = sampleData;//await fsUtils.readFile(fname);
		// if (ans) {
		// 	const response = paverUtils.toJson(ans);
		// 	console.dir(JSON.parse(response), { depth: null });
		// 	this.connection.sendNotification(PrecisaServerCommands.displayRegions, { response, request: data });
		// } else {
		// 	console.error(`[precisa.display-regions] Error: could not read ${fname} file`);
		// }
	}

	/**
	 * Internal function, runs the error bounds analysis with precisa
	 */
	protected async computeErrorBounds (req: PrecisaAnalysisRequest): Promise<Diagnostic> {
		if (req) {
            const msg: string = await execPrecisa(req, { precisaPath: this.precisaPath, kodiakPath: this.kodiakPath }, { connection: this.connection });
            if (msg) {
				// process results
				let match: RegExpMatchArray = /\bstable\s+paths\s*:\s*(.*)/g.exec(msg);
				const errorBound: string = (match && match.length > 1) ?  Number.parseFloat(match[1]).toExponential() : msg;
				const errorBoundInfo: MarkupContent = {
					kind: "markdown",
					value: errorBound
				};
				match = /\bunstable\s*paths\s*:\s*(.*)/g.exec(msg);
				const unstableErrorBound: string = (match && match.length > 1) ? Number.parseFloat(match[1]).toExponential() : null;

				match = /Numeric lemmas and proofs in: (.*)/g.exec(msg);
				const numeric_certificate_file: string = (match && match.length > 1) ? match[1] : null;
				const numericProofFile: DiagnosticRelatedInformation = {
					location: {
						uri: `file://${numeric_certificate_file}`,
						range: {
							start: { line: 5, character: 0 },
							end: { line: 6, character: 0 }
						}
					},
					message: "Certificate file (numeric proofs)"
				};
				match = /Symbolic lemmas and proofs in: (.*)/g.exec(msg);
				const symbolic_certificate_file: string = (match && match.length > 1) ? match[1] : null;
				const symbolicProofFile: DiagnosticRelatedInformation = {
					location: {
						uri: `file://${symbolic_certificate_file}`,
						range: {
							start: { line: 2, character: 0 },
							end: { line: 3, character: 0 }
						}
					},
					message: "Certificate file (symbolic proofs)"
				};
				const fun: string = precisaInputDataToString({ formula: req.formula, input: req.input });
				const res: PrecisaDiagnostic = {
					severity: DiagnosticSeverity.Information,
					range: {
						start: { line: req.position.line, character: 2 },
						end: { line: req.position.line, character: 100 },
					},
					message: Number.parseFloat(errorBoundInfo?.value).toExponential(),
					source: `Round-Off Error for ${fun}`,
					// use diagnostic-related array to point to the pvs proof files
					relatedInformation: [ numericProofFile, symbolicProofFile ],
					errorBound,
					unstableErrorBound
				};
                return res;
			}
		}
		return null;
	}

	/**
	 * Internal function, computes the input variations necessay to perform the sensitivity analysis
	 */
	protected computeUncertaintyVariations (input: PrecisaInputData[]): PrecisaInputData[][] {
		// uncertainty for each input variable
		const uncertainties: number[] = input.map(input => {
			return parseFloat(input.range.uncertainty);
		});
		let variations: PrecisaInputData[][] = [];
		for (let i = 0; i < input.length; i++) {
			const unc: number = uncertainties[i] / 100; // uncertainty for the i-th input
			if (unc > 0) {
				// create the input variations by altering the i-th input by +/- unc
				const min: number = +input[i].range.min;
				const max: number = +input[i].range.max;
				const min_plus: number = min ? (min + min * unc) : unc;
				const min_minus: number = min ? (min - min * unc) : -unc;
				const max_plus: number = max ? (max + max * unc) : unc;
				const max_minus: number = max ? (max - max * unc) : -unc;
				const group: string = `group-${i}`;

				let vari: PrecisaInputData[] = clone(input, { group });
				vari[i].range.min = `${Number.parseFloat(Number.parseFloat(`${min_plus}`).toPrecision(8))}`;
				variations.push(vari);

				vari = clone(input, { group });
				vari[i].range.min = `${Number.parseFloat(Number.parseFloat(`${min_minus}`).toPrecision(8))}`;
				variations.push(vari);

				vari = clone(input, { group });
				vari[i].range.max = `${Number.parseFloat(Number.parseFloat(`${max_plus}`).toPrecision(8))}`
				variations.push(vari);

				vari = clone(input, { group });
				vari[i].range.max = `${Number.parseFloat(Number.parseFloat(`${max_minus}`).toPrecision(8))}`;
				variations.push(vari);
			}
		}
		return variations;
	}
	/**
	 * Internal function, computes the input variations necessay to perform the interval analysis
	 */
	protected computeIntervalsVariations (input: PrecisaInputData[]): PrecisaInputData[][] {
		// number of intervals for each input variable
		const nIntervals: number[] = input.map(input => {
			return parseFloat(input.range.intervals);
		});
		let variations: PrecisaInputData[][] = [];
		for (let i = 0; i < input.length; i++) {
			if (nIntervals[i] > 0) {
				const group: string = `group-${i + 1}`;
				const step: number = Math.abs(+input[i].range.max - +input[i].range.min) / nIntervals[i]; // uncertainty for the i-th input		
				// create the input variations by altering the i-th input using the computed step
				const limit: number = +input[i].range.max;
				for (let k = 0; k < nIntervals[i]; k++) {
					const min: number = +input[i].range.min + k * step;
					const max: number = min + step <= limit ? min + step : limit;
					let vari: PrecisaInputData[] = clone(input, { group });
					vari[i].range.min = `${Number.parseFloat(Number.parseFloat(`${min}`).toPrecision(8))}`;
					vari[i].range.max = `${Number.parseFloat(Number.parseFloat(`${max}`).toPrecision(8))}`;
					variations.push(vari);
				}				
			}
		}
		return variations;
	}

    /**
     * Handler for computing error bounds with PRECiSA
     */
	async errorBoundsAnalysis (req: PrecisaAnalysisRequest): Promise<void> {
		if (req) {
			const diag: PrecisaDiagnostic = await this.computeErrorBounds(req);
			let res: PrecisaAnalysisResponse = {
				...req,
				errorBound: diag?.errorBound,
				unstableErrorBound: diag?.unstableErrorBound,
				//@ts-ignore
				diag
			};
			this.connection.sendNotification(PrecisaServerCommands.computeErrorBounds, { req, res });
			// diags will be sent later, see function saveResults
			// if (res) {
			// 	const fname: string = path.join(req.contextFolder, `${req.fileName}${req.fileExtension}`);
			// 	this.connection.sendDiagnostics({ uri: `file://${fname}`, diagnostics: [ diag ] });
			// }
		}
	}
	/**
	 * Handler for sensitivity analysis for a given uncertainty of input range
	 */
	async sensitivityAnalysis (req: PrecisaAnalysisRequest): Promise<void> {
		if (req) {
			const variations: PrecisaInputData[][] = this.computeUncertaintyVariations(req.input);
			console.log("[precisa-server] sensitivityAnalysis", { req, input: req.input, variations });

			const diag: Diagnostic = await this.computeErrorBounds(req);
			let res: SensitivityAnalysisResponse = {
				...req,
				errorBound: diag?.message,
				sensitivity: []
			};
			for (let i = 0; i < variations.length; i++) {
				const diag: Diagnostic = await this.computeErrorBounds({
					...req,
					input: variations[i]
				});
				res.sensitivity.push({
					input: variations[i],
					errorBound: diag?.message,
					group: variations[i]?.length ? variations[i][0].group : undefined
				});
			}
			console.log("[precisa-server] sensitivity analysis results ready!");
			console.dir({ res });
			this.connection.sendNotification(PrecisaServerCommands.computeSensitivity, { req, res });
		}
	}
	/**
	 * Handler for interval analysis
	 */
	async intervalAnalysis (req: PrecisaAnalysisRequest): Promise<void> {
		if (req) {
			const variations: PrecisaInputData[][] = this.computeIntervalsVariations(req.input);
			console.log("[precisa-server] intervalAnalysis", { req, input: req.input, variations });

			const diag: Diagnostic = await this.computeErrorBounds(req);
			let res: IntervalAnalysisResponse = {
				...req,
				errorBound: diag?.message,
				intervals: []
			};
			for (let i = 0; i < variations.length; i++) {
				const diag: Diagnostic = await this.computeErrorBounds({
					...req,
					input: variations[i]
				});
				res.intervals.push({
					input: variations[i],
					errorBound: diag?.message,
					group: variations[i]?.length ? variations[i][0].group : undefined
				});
			}
			console.log("[precisa-server] interval analysis results ready!");
			console.dir({ res });
			this.connection.sendNotification(PrecisaServerCommands.computeIntervals, { req, res });
		}
	}
    /**
     * Handler for computing comparative analysis with PRECiSA
     */
	async comparativeAnalysis (req: PrecisaComparativeAnalysisRequest): Promise<void> {
		if (req) {
			const diag2: PrecisaDiagnostic = await this.computeErrorBounds({
				...req?.compareWith,
				input: req.input,
				options: req.options
			});
			const compared: PrecisaAnalysisResponse = {
				...req,
				errorBound: diag2?.errorBound,
				unstableErrorBound: diag2?.unstableErrorBound,
				//@ts-ignore
				diag: diag2
			};
			const diag1: PrecisaDiagnostic = await this.computeErrorBounds(req);
			const res: ComparativeAnalysisResponse = {
				...req,
				errorBound: diag1?.errorBound,
				unstableErrorBound: diag1?.unstableErrorBound,
				//@ts-ignore
				diag: diag1,
				compared
			};
			console.log("[precisa-server] comparative analysis results ready!");
			console.dir({ res });
			this.connection.sendNotification(PrecisaServerCommands.computeComparative, { req, res });
			// diags will be sent later, see function saveResults
			// if (res) {
			// 	const fname: string = path.join(req.contextFolder, `${req.fileName}${req.fileExtension}`);
			// 	this.connection.sendDiagnostics({ uri: `file://${fname}`, diagnostics: [ diag ] });
			// }
		}
	}
	/**
	 * Handler for computing paving data
	 */
	async pavingAnalysis (req: PrecisaAnalysisRequest): Promise<void> {
		const paving: PavingData = await this.computePaving(req);
		const res: PavingAnalysisResponse = {
			...req,
			errorBound: null,
			paving
		};
		this.connection.sendNotification(PrecisaServerCommands.computePaving, { req, res });
	}
	/**
	 * Save analysis results and sends diagnostics
	 */
	async saveResults (req: PrecisaSaveResultsRequest): Promise<boolean> {
		const res: PrecisaSaveResultsResponse = { success: false };
		if (req?.results?.diag) {
			//@ts-ignore
			const diag: Diagnostic = req.results.diag;
			const info: DiagnosticRelatedInformation[] = diag.relatedInformation;
			// move files under the current pvs workspace and update diagnostic info accordingly
			for (let i = 0; i < info.length; i++) {
				const fname: string = info[i].location.uri.replace("file://", "");
				const target: string = path.join(req.contextFolder, "cert", fsUtils.getFileName(fname, { keepExtension: true }));
				await fsUtils.copyFile(fname, target);
				info[i].location.uri = `file://${target}`;
			}
			res.success = true;
			// this.connection?.sendDiagnostics({ uri: `file://${fname}`, diagnostics: [ diag ] });
			this.connection?.sendNotification(PrecisaServerCommands.saveResults, ({ req, res }));
			return true;
		}
		this.connection?.sendNotification(PrecisaServerCommands.saveResults, ({ req, res }));
		return false;
	}
}


const toyExample: string = `
## File: VSCodeExample.dat
## Type: 2
## Vars:
               x               y               z
               0               0               0
               1               1             0.1

## Certainly: 23 boxes 
            0.25           0.625           0.025
           0.375            0.75            0.05

            0.25            0.75               0
             0.5               1            0.05

               0             0.5               0
            0.25               1            0.05

            0.25             0.5           0.075
             0.5            0.75             0.1

            0.25           0.625            0.05
           0.375            0.75           0.075

            0.25            0.75            0.05
             0.5               1             0.1

               0             0.5            0.05
            0.25               1             0.1

               0           0.375           0.025
           0.125             0.5            0.05

            0.25               0           0.075
             0.5            0.25             0.1

            0.25            0.25           0.075
             0.5             0.5             0.1

               0               0           0.075
            0.25            0.25             0.1

               0            0.25           0.075
            0.25             0.5             0.1

               0           0.375            0.05
           0.125             0.5           0.075

             0.5           0.875           0.025
           0.625               1            0.05

            0.75             0.5           0.075
               1            0.75             0.1

            0.75            0.75           0.075
               1               1             0.1

             0.5             0.5           0.075
            0.75            0.75             0.1

             0.5            0.75           0.075
            0.75               1             0.1

             0.5           0.875            0.05
           0.625               1           0.075

            0.75               0           0.075
               1            0.25             0.1

            0.75            0.25           0.075
               1             0.5             0.1

             0.5               0           0.075
            0.75            0.25             0.1

             0.5            0.25           0.075
            0.75             0.5             0.1

## Possibly: 47 boxes 
            0.25             0.5           0.025
           0.375           0.625            0.05

           0.375             0.5           0.025
             0.5            0.75            0.05

           0.375             0.5               0
             0.5           0.625           0.025

            0.25             0.5            0.05
           0.375           0.625          0.0625

           0.375           0.625            0.05
             0.5            0.75          0.0625

           0.375             0.5            0.05
             0.5           0.625          0.0625

            0.25           0.125           0.025
           0.375            0.25            0.05

            0.25           0.125               0
           0.375            0.25           0.025

            0.25            0.25               0
             0.5             0.5            0.05

               0               0               0
            0.25            0.25            0.05

               0            0.25           0.025
           0.125           0.375            0.05

           0.125            0.25           0.025
            0.25             0.5            0.05

           0.125            0.25               0
            0.25           0.375           0.025

            0.25           0.125            0.05
           0.375            0.25          0.0625

            0.25           0.375            0.05
           0.375             0.5          0.0625

            0.25            0.25            0.05
           0.375           0.375          0.0625

           0.375           0.375            0.05
             0.5             0.5          0.0625

           0.375            0.25            0.05
             0.5           0.375          0.0625

               0           0.125            0.05
           0.125            0.25          0.0625

               0               0            0.05
           0.125           0.125          0.0625

           0.125           0.125            0.05
            0.25            0.25          0.0625

           0.125               0            0.05
            0.25           0.125          0.0625

               0            0.25            0.05
           0.125           0.375          0.0625

           0.125           0.375            0.05
            0.25             0.5          0.0625

           0.125            0.25            0.05
            0.25           0.375          0.0625

            0.75           0.625           0.025
           0.875            0.75            0.05

            0.75           0.625               0
           0.875            0.75           0.025

            0.75            0.75               0
               1               1            0.05

             0.5             0.5               0
            0.75            0.75            0.05

             0.5            0.75           0.025
           0.625           0.875            0.05

           0.625            0.75           0.025
            0.75               1            0.05

           0.625            0.75               0
            0.75           0.875           0.025

            0.75           0.625            0.05
           0.875            0.75          0.0625

            0.75           0.875            0.05
           0.875               1          0.0625

            0.75            0.75            0.05
           0.875           0.875          0.0625

           0.875           0.875            0.05
               1               1          0.0625

           0.875            0.75            0.05
               1           0.875          0.0625

             0.5           0.625            0.05
           0.625            0.75          0.0625

             0.5             0.5            0.05
           0.625           0.625          0.0625

           0.625           0.625            0.05
            0.75            0.75          0.0625

           0.625             0.5            0.05
            0.75           0.625          0.0625

             0.5            0.75            0.05
           0.625           0.875          0.0625

           0.625           0.875            0.05
            0.75               1          0.0625

           0.625            0.75            0.05
            0.75           0.875          0.0625

             0.5           0.375           0.025
           0.625             0.5            0.05

             0.5           0.375               0
           0.625             0.5           0.025

             0.5           0.375            0.05
           0.625             0.5          0.0625

## Almost Certainly: 55 boxes 
            0.25             0.5               0
           0.375            0.75           0.025

           0.375           0.625               0
             0.5            0.75           0.025

            0.25             0.5          0.0625
           0.375           0.625           0.075

           0.375           0.625          0.0625
             0.5            0.75           0.075

           0.375             0.5          0.0625
             0.5           0.625           0.075

               0            0.25               0
           0.125             0.5           0.025

           0.125           0.375               0
            0.25             0.5           0.025

            0.25           0.125          0.0625
           0.375            0.25           0.075

            0.25               0          0.0625
           0.375           0.125           0.075

           0.375           0.125          0.0625
             0.5            0.25           0.075

           0.375               0          0.0625
             0.5           0.125           0.075

            0.25           0.375          0.0625
           0.375             0.5           0.075

            0.25            0.25          0.0625
           0.375           0.375           0.075

           0.375           0.375          0.0625
             0.5             0.5           0.075

           0.375            0.25          0.0625
             0.5           0.375           0.075

               0           0.125          0.0625
           0.125            0.25           0.075

               0               0          0.0625
           0.125           0.125           0.075

           0.125           0.125          0.0625
            0.25            0.25           0.075

           0.125               0          0.0625
            0.25           0.125           0.075

               0            0.25          0.0625
           0.125           0.375           0.075

           0.125           0.375          0.0625
            0.25             0.5           0.075

           0.125            0.25          0.0625
            0.25           0.375           0.075

             0.5            0.75               0
           0.625               1           0.025

           0.625           0.875               0
            0.75               1           0.025

            0.75           0.625          0.0625
           0.875            0.75           0.075

            0.75             0.5          0.0625
           0.875           0.625           0.075

           0.875           0.625          0.0625
               1            0.75           0.075

           0.875             0.5          0.0625
               1           0.625           0.075

            0.75           0.875          0.0625
           0.875               1           0.075

            0.75            0.75          0.0625
           0.875           0.875           0.075

           0.875           0.875          0.0625
               1               1           0.075

           0.875            0.75          0.0625
               1           0.875           0.075

             0.5           0.625          0.0625
           0.625            0.75           0.075

             0.5             0.5          0.0625
           0.625           0.625           0.075

           0.625           0.625          0.0625
            0.75            0.75           0.075

           0.625             0.5          0.0625
            0.75           0.625           0.075

             0.5            0.75          0.0625
           0.625           0.875           0.075

           0.625           0.875          0.0625
            0.75               1           0.075

           0.625            0.75          0.0625
            0.75           0.875           0.075

            0.75           0.125          0.0625
           0.875            0.25           0.075

            0.75               0          0.0625
           0.875           0.125           0.075

           0.875           0.125          0.0625
               1            0.25           0.075

           0.875               0          0.0625
               1           0.125           0.075

            0.75           0.375          0.0625
           0.875             0.5           0.075

            0.75            0.25          0.0625
           0.875           0.375           0.075

           0.875           0.375          0.0625
               1             0.5           0.075

           0.875            0.25          0.0625
               1           0.375           0.075

             0.5           0.125          0.0625
           0.625            0.25           0.075

             0.5               0          0.0625
           0.625           0.125           0.075

           0.625           0.125          0.0625
            0.75            0.25           0.075

           0.625               0          0.0625
            0.75           0.125           0.075

             0.5           0.375          0.0625
           0.625             0.5           0.075

             0.5            0.25          0.0625
           0.625           0.375           0.075

           0.625           0.375          0.0625
            0.75             0.5           0.075

           0.625            0.25          0.0625
            0.75           0.375           0.075

## Certainly Not: 35 boxes 
            0.25               0           0.025
           0.375           0.125            0.05

           0.375               0           0.025
             0.5            0.25            0.05

            0.25               0               0
           0.375           0.125           0.025

           0.375               0               0
             0.5            0.25           0.025

            0.25               0            0.05
           0.375           0.125          0.0625

           0.375           0.125            0.05
             0.5            0.25          0.0625

           0.375               0            0.05
             0.5           0.125          0.0625

            0.75             0.5           0.025
           0.875           0.625            0.05

           0.875             0.5           0.025
               1            0.75            0.05

            0.75             0.5               0
           0.875           0.625           0.025

           0.875             0.5               0
               1            0.75           0.025

            0.75             0.5            0.05
           0.875           0.625          0.0625

           0.875           0.625            0.05
               1            0.75          0.0625

           0.875             0.5            0.05
               1           0.625          0.0625

            0.75               0               0
               1             0.5            0.05

             0.5               0               0
            0.75            0.25            0.05

             0.5            0.25           0.025
           0.625           0.375            0.05

           0.625            0.25           0.025
            0.75             0.5            0.05

             0.5            0.25               0
           0.625           0.375           0.025

           0.625            0.25               0
            0.75             0.5           0.025

            0.75           0.125            0.05
           0.875            0.25          0.0625

            0.75               0            0.05
           0.875           0.125          0.0625

           0.875           0.125            0.05
               1            0.25          0.0625

           0.875               0            0.05
               1           0.125          0.0625

            0.75           0.375            0.05
           0.875             0.5          0.0625

            0.75            0.25            0.05
           0.875           0.375          0.0625

           0.875           0.375            0.05
               1             0.5          0.0625

           0.875            0.25            0.05
               1           0.375          0.0625

             0.5           0.125            0.05
           0.625            0.25          0.0625

             0.5               0            0.05
           0.625           0.125          0.0625

           0.625           0.125            0.05
            0.75            0.25          0.0625

           0.625               0            0.05
            0.75           0.125          0.0625

             0.5            0.25            0.05
           0.625           0.375          0.0625

           0.625           0.375            0.05
            0.75             0.5          0.0625

           0.625            0.25            0.05
            0.75           0.375          0.0625
`;

// instantiate the language server
new VSCodePrecisaServer();
