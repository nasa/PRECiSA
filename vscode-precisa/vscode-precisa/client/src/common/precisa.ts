/**
 * @module PrecisaDeclarations
 * @author Paolo Masci
 * @date 2021.11.06
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

import { Diagnostic } from "vscode";
import { PavingData } from "./paverUtils";

/**
 * Requests/Notifications exchanged with the server
 */
export enum PrecisaServerCommands {
    failure = "precisa.failure",
    analysisError = "precisa.analysis-error",
    startServer = "precisa.start-server",
    stopServer = "precisa.stop-server",
    computeErrorBounds = "precisa.compute-error-bounds",
    computeSensitivity = "precisa.compute-sensitivity",
    computeIntervals = "precisa.compute-intervals",
    computeComparative = "precisa.compute-comparative",
    saveResults = "precisa.save-results",
    computePaving = "precisa.compute-paving",
    precisaNotFound = "precisa.not-found"
}

/**
 * Formula descriptor
 */
 export declare interface FormulaDesc {
    contextFolder: string,
    fileName: string,
    fileExtension: string,
    theory: string,
    formula: string,
    position: { 
        line: number,
        character: number,
        index: number
    },
    annotation: string
}
/**
 * Input data for precisa
 */
export declare interface PrecisaInputData {
    name: string, 
    range: { min: string, max: string, uncertainty?: string, intervals?: string },
    group?: string // this can be used to assign the input data to a conceptual group
}
/**
 * Precisa options
 */
export declare interface PrecisaOptions {
    depth: number,
    precision: number,
    selected?: string[] // selected variables
}
/**
 * Request sent to precisa server to perform the analysis
 */
export declare interface PrecisaAnalysisRequest extends FormulaDesc {
    input: PrecisaInputData[],
    options: PrecisaOptions
}
/**
 * Request sent to precisa server to perform the analysis
 */
export declare interface PrecisaComparativeAnalysisRequest extends PrecisaAnalysisRequest {
    compareWith: FormulaDesc // function to be compared with
}
/**
 * Comparative analysis response
 */
export declare interface ComparativeAnalysisResponse extends PrecisaAnalysisResponse {
    compareWith: FormulaDesc,
    compared: { // results for the compared function
        input: PrecisaInputData[],
        errorBound: string,
        unstableErrorBound?: string
    }
}
/**
 * Request to start/restart precisa
 */
export declare interface StartPrecisaRequest {
    precisaPath: string, 
    kodiakPath: string
}
/**
 * Request sent to precisa server to save results
 */
export declare interface PrecisaSaveResultsRequest extends FormulaDesc {
    results: PrecisaAnalysisResponse
}
export declare interface PrecisaSaveResultsResponse {
    success: boolean
}
/**
 * Error analysis response
 */
export declare interface PrecisaAnalysisResponse extends PrecisaAnalysisRequest {
    errorBound: string,
    unstableErrorBound?: string,
    diag?: Diagnostic
}
/**
 * Sensitivity analysis response
 */
export declare interface SensitivityAnalysisResponse extends PrecisaAnalysisResponse {
    sensitivity: {
        input: PrecisaInputData[],
        errorBound: string,
        group?: string // this can be used to group together results, e.g., based on which input variable is altered
    }[]
}
/**
 * Intervals analysis response
 */
export declare interface IntervalAnalysisResponse extends PrecisaAnalysisResponse {
    intervals: {
        input: PrecisaInputData[],
        errorBound: string,
        group?: string // this can be used to group together results, e.g., based on which input variable is altered
    }[]
}
/**
 * Paving analysis response
 */
export declare interface PavingAnalysisResponse extends PrecisaAnalysisResponse {
    paving: PavingData
}
