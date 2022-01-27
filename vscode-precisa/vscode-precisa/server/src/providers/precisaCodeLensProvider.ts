
/**
 * @module PrecisaCodeLensProvider
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

import { CancellationToken, CodeLens, CodeLensRequest, Range, CodeLensParams } from 'vscode-languageserver';
import * as fsUtils from '../common/fsUtils';
import { VSCodePrecisaServer } from '../vscodePrecisaServer';
import * as utils from '../common/precisaUtils';
import { FormulaDesc } from '../common/precisa';
import { fpRangeRegex, rangeInfoRegex } from '../common/precisaUtils';

export class PrecisaCodeLensProvider {
    protected precisaServer: VSCodePrecisaServer;


    /**
     * Constructor
     */
    constructor (precisaServer: VSCodePrecisaServer) {
        this.precisaServer = precisaServer;
    }

    /**
	 * Logic of the codelens
	 */
	provideCodeLens(document: { txt: string, uri: string }, token?: CancellationToken): Thenable<CodeLens[]> {
        const codeLens: CodeLens[] = [];
        if (document?.txt) {
            let fileName: string = fsUtils.getFileName(document.uri);
            const fileExtension: string = fsUtils.getFileExtension(document.uri);
            const contextFolder: string = fsUtils.getContextFolder(document.uri);
            
            if (fileExtension === ".pvs") {
                let match: RegExpMatchArray = null;
                const content: string = document.txt;
                
                // group 1 is the annotation
                // group 2 is the function name
				const regex: RegExp = new RegExp(utils.fpFunctionRegex);
                while (match = regex.exec(content)) {
                    if (match && match.length > 2) {
                        const annotation: string = match[1];
                        const formula: string = match[2];
                        const lines: string[] = content.slice(0, match.index + annotation.length).split("\n");
                        const line: number = lines.length - 1; // line is zero-based
                        const character: number = lines[line].length;
                        const position: { line: number, character: number, index: number } = { line, character, index: match.index };
                        const theory: string = content; // we can give precisa the entire theory

                        // codelens
                        const range: Range = {
                            start: { line, character },
                            end: { line, character: character + match[0].length }
                        };
                        const args: FormulaDesc = { contextFolder, fileName, fileExtension, theory, formula, position, annotation };
                        codeLens.push({
                            range,
                            command: {
                                title: "estimate-error-bounds",
                                command: "vscode-precisa.show-analysis-view",
                                arguments: [ args ]
                            }
                        });
                        codeLens.push({
                            range,
                            command: {
                                title: "compare-error-bounds",
                                command: "vscode-precisa.show-split-view",
                                arguments: [ args ]
                            }
                        });
                    }
                }
            }
            return Promise.resolve(codeLens);
        }
        return Promise.resolve([]);
    }

    /**
	 * Codelens promise, not used for now
	 */
    resolveCodeLens(codeLens: CodeLens, token?: CancellationToken): CodeLens {
        return codeLens;
    }

    /**
	 * Standard API of the language server
	 */
    async onCodeLens (args: CodeLensParams, txt: string): Promise<CodeLens[]> {
        const uri: string = args?.textDocument?.uri;
        if (fsUtils.isPvsFile(uri)) {
            let codelens: CodeLens[] = await this.provideCodeLens({ txt, uri });
            return codelens;
        }
        return null;
    }
 }