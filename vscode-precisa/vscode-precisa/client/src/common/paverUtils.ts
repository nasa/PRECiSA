/**
 * @module PaverUtils
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

// Paving corners can be interpreted as range of values
// the first pair is (x1, x2)
// the second pair is (y1, y2)
// the third pair is (z1, z2)
export declare type VarRange = [ string, string ]; // each corner defines a range pair from-to
export declare type PavingCorners = VarRange[]; // one corner for each variable
export declare interface PavingData {
    variables: string[],
    certainly: PavingCorners[],
    possibly: PavingCorners[],
    almost_certainly: PavingCorners[],
    certainly_not: PavingCorners[]
};

export declare type Mesh3DCorners = [ 
	number|string, number|string, number|string, number|string, 
	number|string, number|string, number|string, number|string
];
export declare interface Mesh3DPlot {
	type: "mesh3d",
	x: Mesh3DCorners,
	y: Mesh3DCorners,
	z: Mesh3DCorners,
	opacity: number,
	color: string,
	i: [ 7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2 ],
	j: [ 3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3 ],
	k: [ 0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6 ],
	hoverinfo: string,
	hovertemplate?: string, // this overrides hoverinfo
	text?: string
};
export function makeMesh3D (corners: PavingCorners, height: number, color: string): Mesh3DPlot {
	if (corners?.length) {
		const x: [ string, string ] = corners[0];
		const y: [ string, string ] = corners[1];
		return {
			type: "mesh3d",
			x: [ x[0], x[1], x[1], x[0], x[0], x[1], x[1], x[0] ],
			y: [ y[1], y[1], y[0], y[0], y[1], y[1], y[0], y[0] ],
			z: [ 0, 0, 0, 0, height, height, height, height ],
			opacity: 0.8,
			color,
			i: [ 7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2 ],
			j: [ 3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3 ],
			k: [ 0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6 ],
			hoverinfo: "x+y",
			hovertemplate: "[%{x}, %{y}]<extra></extra>",
			text: ""
		};
	}
	return null;
}

/**
 * Converts a paving file to json format.
 * See sampleData below for an example paving file content.
 */
export function paving2Json (paving: string): PavingData {
    const varNames: RegExp = /## Vars:\s*([^\n]*)/;
    const match_varNames: RegExpMatchArray = varNames.exec(paving);
    if (match_varNames && match_varNames.length > 1) {
        const variableNames: string[] = match_varNames[1].replace(/ +/g, " ").split(" ").map(elem => { return elem.trim(); }).filter(elem => { return elem.length > 0; });
        const certainly: RegExp = /## Certainly:\s*(\d+).*\n\s*([\d\.\s\-]+)/g;
        const possibly: RegExp = /## Possibly:\s*(\d+).*\n\s*([\d\.\s\-]+)/g;
        const almost_certainly: RegExp = /## Almost Certainly:\s*(\d+).*\n\s*([\d\.\s\-]+)/g;
        const certainly_not: RegExp = /## Certainly Not:\s*(\d+).*\n\s*([\d\.\s\-]+)/g;
        const regs: { [key: string]: RegExp } = {
            certainly,
            possibly,
            almost_certainly,
            certainly_not 
        };
        const ans: PavingData = {
            variables: variableNames,
            certainly: [],
            possibly: [],
            almost_certainly: [],
            certainly_not: []
        };
        const keys: string[] = Object.keys(ans);
        for (let k = 0; k < keys.length; k++) {
            if (regs[keys[k]]) {
                let match: RegExpMatchArray = regs[keys[k]].exec(paving);
                if (match && match.length > 2) {
                    const lines: string[] = match[2].replace(/ +/g, " ").split("\n").filter(elem => { return elem.trim() !== ""; }).map(elem => { return elem.trim(); });
                    for (let i = 1; i < lines.length; i+=2) {
                        const points: PavingCorners = [];
                        const line_1: string[] = lines[i - 1].split(" ").map(elem => { return elem.trim(); });
                        const line_2: string[] = lines[i].split(" ").map(elem => { return elem.trim(); });
                        for (let p = 0; p < line_1.length && p < line_2.length; p++) {
                            points.push([ line_1[p], line_2[p] ]); // array of points
                        }
                        if (points.length === variableNames.length) {
                            ans[keys[k]].push(points);
                        } else {
                            console.error("[paverUtils.toJson] Warning: error while parsing paver data (incorrect number of lines)");
                        }
                    }
                }
            }
        }
        return ans;
    }
    return null;
}

/**
 * Creates a 2D plot for given paving data
 */
export function htmlPlot (
	pavingInfo: PavingData, 
	range: { 
		x: { min: string | number, max: string | number }, 
		y: { min: string | number, max: string | number }
	}
): string {
    const width: number = 400;
    const height: number = 400;
    const x_range: number = Math.abs(+range.x.max - +range.x.min);
    const y_range: number = Math.abs(+range.y.max - +range.y.min);
    const ratio_x: number = width / x_range;
    const ratio_y: number = height / y_range;
    const render = (data: PavingData, layer: string, opt?: { color?: string, col1?: number, col2?: number }): string => {
        if (data) {
            opt = opt || {};
            const color: string = opt.color || "black";
            const col0: number = isNaN(opt.col1) ? 0 : opt.col1;
            const col1: number = isNaN(opt.col2) ? 1 : opt.col2;
            const divs: string[] = [];
            for (let i = 0; i < data[layer].length; i++) {
                const x1: number = +data[layer][i][col0][0];
                const x2: number = +data[layer][i][col0][1];
                const y1: number = +data[layer][i][col1][0];
                const y2: number = +data[layer][i][col1][1];
                const sector_width: number = (x1 > x2) ? (x1 - x2) * ratio_x : (x2 - x1) * ratio_x;
                const sector_height: number = (y1 > y2) ? (y1 - y2) * ratio_y : (y2 - y1) * ratio_y;
                const sector_left: number = x1 * ratio_x;
                const sector_top: number = y1 * ratio_y;
                const top: number = height - sector_height - sector_top; // we need to flip the y axis so 0,0 is the bot left corner of the diagram
                const divElem = `<div style='position:absolute; left:${sector_left}px; top:${top}px; width:${sector_width}px; height:${sector_height}px; background-color:${color};'></div>`;          
                divs.push(divElem);
            }
            return divs.join("\n");
        }
        return null;
    }
    const certainlyLayer: string = render(pavingInfo, "certainly", { color: "lime" });
    const possiblyLayer: string = render(pavingInfo, "possibly", { color: "red" });
    const almostCertainlyLayer: string = render(pavingInfo, "almost_certainly", { color: "blue"});
    const certainlyNotLayer: string = render(pavingInfo, "certainly_not", { color: "gray"});
    const html: string = `
<div style="position:relative;">
<div id="certainly" style="position:absolute; width:${width}px; height:${height}px; top:0; left:0;">
`
+ certainlyLayer +
`
</div>
<div id="possibly" style="position:absolute; width:${width}px; height:${height}px; top:0; left:0;">
`
+ possiblyLayer +
`
</div>
<div id="almost_certainly" style="position:absolute; width:${width}px; height:${height}px; top:0; left:0;">
`
+ almostCertainlyLayer +
`
</div>
<div id="certainly_not" style="position:absolute; width:${width}px; height:${height}px; top:0; left:0;">
`
+ certainlyNotLayer +
`
</div>
</div>
    `;
    console.log(html);
    return html;
};