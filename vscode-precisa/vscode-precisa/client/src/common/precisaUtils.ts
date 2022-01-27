/**
 * @module PrecisaUtils
 * @author Paolo Masci
 * @date 2021.12.11
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

import { PrecisaInputData } from "./precisa";

// precisa options
export const DEFAULT_PRECISION: number = 14;
export const DEFAULT_DEPTH: number = 7;

// group 1 is the function name
// group 2 is the function args
// group 3 is the function return type
//@deprecated -- this regex is not robust, as it may capture expressions in the body of a pvs function
export const pvsFunctionDefinition: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*\(([\w\W]*)\)\s*:\s*(\w+\?*)\s*=/g;

// this regex describes the structure of a valid function name
export const pvsIdentifier: RegExp = /[A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*/g;

// fpFunction captures the annotation @fp-function
// the use of annotations is good for promoting appropriate code documentation
// group 1 is the annotation + any comment line between the annotation and the function name
// group 2 is the function name
// if needed, args can be found by scanning the text after the regexp, until open/close brackets match
export const fpFunctionRegex: RegExp = /((?:%\s*@\s*fp-function\b.*\s*)(?:%.*\s*)*)([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*\(/g;

// fpRange captures range annotations such as @fp-range X in [1,2], Y in [3,4]
// group 1 is the range information for the variables
export const fpRangeRegex: RegExp = /(?:%\s*@\s*fp-range\s+)(.*)/g;

// rangeInfo captures range information such as X in [1,2], Y in [3,4]
// group 1 is the variable name
// group 2 is the variable range
export const rangeInfoRegex: RegExp = /([A-Za-z][\w\?₀₁₂₃₄₅₆₇₈₉]*)\s*in\s*\[([^\]]+)\]/g;

// group 1 is a comma-separated list of arguments
export const pvsFunctionArgs: RegExp = /([\s\w\?:,]+)/g;

// pvs comments
export const commentRegexp: RegExp = /%.*/g;

// get indexes of first and last matching parens
export function matchingParens (txt: string): { start: number, end: number } {
    if (txt) {
        let start: number = -1;
        let end: number = -1;
        let nopen: number = 0;
        for (let i = 0; i < txt.length && end === -1 && nopen >=0 ; i++) {
            switch (txt[i]) {
                case '(': {
                    start = start === -1 ? i : start;
                    nopen++;
                    break;
                }
                case ')': {
                    nopen--;
                    end = nopen === 0 ? i : end;
                    break;
                }
                default: {
                    break;
                }
            }
        }
        return start !== -1 && end !== -1 && end > start ? { start, end } : null;
    }
    return null;
}

/**
 * Prints input data in the format accepted by precisa
 */
export function precisaInputDataToString (desc: { formula: string, input: PrecisaInputData[] }): string {
    if (desc) {
        return `${precisaFunctionToString(desc)}: ${precisaRangeToString(desc)}`;
    }
    return "";
}

/**
 * Prints the function name in the format accepted by precisa
 */
export function precisaFunctionToString (desc: { formula: string, input: PrecisaInputData[] }): string {
    if (desc) {
        return `${desc.formula}(${desc.input.map(elem => {
            return elem.name;
        }).join(", ")})`;
    }
    return "";
}

/**
 * Prints the range of the input data in the format accepted by precisa
 */
export function precisaRangeToString (desc: { input: PrecisaInputData[] }): string {
    if (desc) {
        return `${desc.input.map(elem => {
            return `${elem.name} in [ ${elem.range.min}, ${elem.range.max} ]`;
        }).join(", ")}`;
    }
    return "";
}
/**
 * Annotations
 */
export const annotations: { [key: string]: string } = {
    fp_error: "@fp-error"
};
/**
 * Shortens a message to a given length by removing fragments of the central part 
 * of the message. Useful to shorten error messages when they are overly long.
 */
export function formatMessage (msg: string, maxLen?: number): string {
    maxLen = maxLen || DEFAULT_ERROR_MESSAGE_LEN;
    if (msg?.length > maxLen) {
        const start: string = msg.substring(0, maxLen / 2);
        const end: string = msg.substring(msg.length - maxLen / 2);
        return start + " ... " + end;
    }
    return msg;
}
export const DEFAULT_ERROR_MESSAGE_LEN: number = 80; // 80 characters