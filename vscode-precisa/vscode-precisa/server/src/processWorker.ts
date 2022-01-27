/**
 * @module ProcessWorker
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

import { PrecisaAnalysisRequest, PrecisaServerCommands } from "./common/precisa";
import * as path from 'path';
import { execSync } from 'child_process';
import * as os from 'os';
import * as fsUtils from './common/fsUtils';
import { DEFAULT_DEPTH, DEFAULT_PRECISION, formatMessage, precisaInputDataToString } from "./common/precisaUtils";
import { Connection } from "vscode-languageserver";

/**
 * PRECiSA process worker, executes precisa with given args
 * Example invocations
 * ./precisa analyze ../benchmarks/analysis/FPBench/carbonGas.pvs ../benchmarks/analysis/FPBench/carbonGas.input
 * ./precisa analyze --paving --json ../benchmarks/analysis/FPBench/carbonGas.pvs ../benchmarks/analysis/FPBench/carbonGas.input`
 */
export async function execPrecisa (req: PrecisaAnalysisRequest, config: { precisaPath: string, kodiakPath: string }, opt?: { paving?: boolean, connection?: Connection }): Promise<string | null> {
    opt = opt || {};
    const options: string = opt.paving ? "--paving" : "";
    const precisa: string = path.join(config?.precisaPath, 'precisa');
    
    // create temporary files in a dedicated folder under the temp dir
    const folderName: string = path.join(os.tmpdir(), `${new Date().getTime()}`);
    await fsUtils.createFolder(folderName);
    const fnamePvs: string = path.join(folderName, `${req.fileName}.pvs`);
    const fnameInput: string = path.join(folderName, `${req.fileName}.input`);
    await fsUtils.writeFile(fnamePvs, req.theory);

    // input accepted by precisa is in the form e1(X, Y): X in [ 3, 4 ], Y in [ 0, 100 ]
    const input: string = precisaInputDataToString({ formula: req.formula, input: req.input });
    await fsUtils.writeFile(fnameInput, input);

    let libs: string[] = process.env["LD_LIBRARY_PATH"]?.split(":") || [];
    if (config?.kodiakPath && libs.indexOf(config?.kodiakPath) < 0) {
        libs = [ config?.kodiakPath ].concat(libs);
    }
    const LD_LIBRARY_PATH: string = libs.join(":");
    console.log(`LD_LIBRARY_PATH = ${LD_LIBRARY_PATH}`);

    // compose command for executing precisa
    const depth: number = req?.options?.depth >= 1 ? Math.floor(req.options.depth) : DEFAULT_DEPTH;
    const precision: number = req?.options?.precision >= 1 ? Math.floor(req.options.precision) : DEFAULT_PRECISION;
    const cmd: string = `export LD_LIBRARY_PATH=${LD_LIBRARY_PATH} && ${precisa} analyze ${fnamePvs} ${fnameInput} -d ${depth} -p ${precision} ${options}`;
    console.log(cmd);

    // wait for results
    try {
        // execute precisa
        const ans: Buffer = execSync(cmd);
        const res: string = ans?.toLocaleString() || "";
        console.log(res);
        return res;
    } catch (precisa_error) {
        const msg: string = precisa_error.stderr?.toLocaleString();
        console.error("[precisa-server] Unable to compute error bounds", msg);
        const matchErrorKodiakEval: RegExpMatchArray = /\bKodiak\s*\(eval\):(.+)/g.exec(msg);
        const matchErrorKodiakCheckIndex: RegExpMatchArray = /\bKodiak\s*:\s*\(checkIndexed\)(.+)/g.exec(msg);
        const matchSyntaxError: RegExpMatchArray = /\bprecisa:\s*(.*)/g.exec(msg);

        const connection: Connection = opt?.connection;
        if (matchErrorKodiakEval?.length) {
            connection?.sendNotification(PrecisaServerCommands.analysisError, { msg: "Error:" + formatMessage(matchErrorKodiakEval[1]) });
        } else if (matchErrorKodiakCheckIndex?.length) {
            connection?.sendNotification(PrecisaServerCommands.analysisError, { msg: "Error:" + formatMessage(matchErrorKodiakCheckIndex[1]) });
        } else if (matchSyntaxError?.length) {
            connection?.sendNotification(PrecisaServerCommands.analysisError, { msg: "Error:" + formatMessage(matchSyntaxError[1]) });
        } else {
            connection?.sendNotification(PrecisaServerCommands.failure, { msg });
        }
    }
    return null;
}