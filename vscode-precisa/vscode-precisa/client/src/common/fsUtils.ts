/**
 * @module fsUtils
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

import * as fs from 'fs';
import * as path from 'path';

const HOME_DIR: string = require('os').homedir();
// nodeJS does not support tilde expansion for the home folder
export function tildeExpansion(path: string): string {
	if (path && (path.startsWith("~/") || path === "~")) {
		path = path.replace("~", HOME_DIR);
	}
	return path;
}
export async function stat(fileName: string): Promise<fs.Stats> {
	if (fileName) {
		fileName = tildeExpansion(fileName);
		return new Promise<fs.Stats>((resolve, reject) => {
			fs.stat(fileName, (error, stat) => {
				// ignore errors for now
				resolve(stat);
			});
		});
	}
	return null;
};
export async function readDir(contextFolder: string): Promise<string[]> {
	if (contextFolder) {
		contextFolder = tildeExpansion(contextFolder);
		return new Promise<string[]>((resolve, reject) => {
			fs.readdir(contextFolder, (error, children) => {
				// ignore errors for now
				resolve(children || []);
			});
		});
	}
	return null;
}
export async function readFile(path: string): Promise<string | null> {
	if (path) {
		path = path.replace("file://", "");
		path = tildeExpansion(path);
		try {
			return fs.readFileSync(path).toString('utf8');
		} catch (fileReadError) {
			// console.error(fileReadError);
			return null;
		}
	}
	return null;
}
export function deleteFile(fname: string): boolean {
	try {
		fs.unlinkSync(fname);
	} catch (deleteError) {
		return false;
	}
	return true;
}
export async function copyFile(from: string, to: string): Promise<boolean> {
	try {
		await createFolder(getContextFolder(to));
		execSync(`rsync ${from} ${to}`);
	} catch (copyError) {
		return false;
	}
	return true;
}
export async function deletePvsCache(contextFolder: string): Promise<boolean> {
	try {
		// console.log(`Deleting cache for context ${contextFolder}`);
		if (contextFolder) {
			contextFolder = tildeExpansion(contextFolder);
			// console.log(`Deleting cache for context ${contextFolder}`);
			const cacheFolder: string = path.join(contextFolder, "pvsbin");
			const pvsBinFiles: string[] = fs.readdirSync(cacheFolder);
			// console.log(pvsBinFiles);
			pvsBinFiles.forEach(file => {
				deleteFile(path.join(cacheFolder, file));
			});
			// console.log(`removing ${path.join(contextFolder, ".pvscontext")}`);
			deleteFile(path.join(contextFolder, ".pvscontext"));
			// console.log(`reading folder ${contextFolder}`);
			const tccFiles: string[] = fs.readdirSync(contextFolder);
			// console.log(tccFiles);
			if (tccFiles) {
				// console.log(tccFiles);
				tccFiles.filter(name => {
					// console.log(name);
					return name.endsWith(".tccs");
				}).forEach(file => {
					// console.log(`deleting ${file}`);
					deleteFile(path.join(cacheFolder, file));
				});
			}
		}
	} catch (deleteError) {
		return false;
	}
	return true;
}
export async function createFolder(path: string): Promise<void> {
	if (!fs.existsSync(path)){
		fs.mkdirSync(path);
	}
}
export async function writeFile(path: string, content: string): Promise<boolean> {
	if (path) {
		try {
			path = tildeExpansion(path);
			fs.writeFileSync(path, content);
		} catch (error) {
			console.warn("[fsUtils] Warning: unable to write file", error);
			return false;
		}
		return true;
	}
	return false;
}
export function getFileName(fname: string, opt?: { keepExtension?: boolean }): string {
	if (fname) {
		fname = fname.replace("file://", "");
		fname = fname.includes("/") ? fname.split("/").slice(-1)[0] : fname;
		if (!opt?.keepExtension) {
			fname = fname.includes(".") ? fname.split(".").slice(0, -1).join(".") : fname;
		}
		return fname;
	}
	return null;
}
export function removeFileExtension(fname: string): string {
	if (fname) {
		return fname.split(".").slice(0, -1).join(".");
	}
	return null;
}
export function getFileExtension(fname: string): string {
	if (fname) {
		return `.${fname.split(".").slice(-1).join(".")}`;
	}
	return null;
}
export function getContextFolder(fname: string): string {
	if (fname) {
		fname = fname.replace("file://", "");
		return fname.split("/").slice(0, -1).join("/").replace("//", "/");
	}
	return null;
}
export function isPvsFile(fileName: string): boolean {
	if (fileName) {
		return fileName.endsWith('.pvs') || fileName.endsWith('.tccs') || fileName.endsWith('.ppe') || fileName.endsWith('.pr');
	}
	return false;
}
export function fileExists(path: string): boolean {
	return pathExists(path);
}
export function dirExists(contextFolder: string): boolean {
	return pathExists(contextFolder);
}
export function folderExists(contextFolder: string): boolean {
	return dirExists(contextFolder);
}

export function pathExists(path: string): boolean {
	let ans: boolean = false;
	if (path) {
		path = tildeExpansion(path);
		try {
			ans = fs.existsSync(path);
			return ans;
		} catch (readError) {
			// console.error(readError);
		}
	}
	return false;
}


export function normalizePath(p: string) {
	if (p) {
		p = path.normalize(p);
		p = (p.endsWith("/")) ? p.substr(0, p.length - 1) : p;
		p = tildeExpansion(p);
	}
	return p;
}

export function getText(txt: string, range: { start: { line: number, character?: number }, end: { line: number, character?: number } }): string {
	if (txt) {
		const lines: string[] = txt.split("\n");
		let ans: string[] = lines.slice(range.start.line, range.end.line + 1);
		if (ans && ans.length > 0) {
			if (!isNaN(range.start.character)) {
				ans[0] = ans[0].substr(range.start.character);
			}
			if (!isNaN(range.end.character)) {
				let endCharacter: number = range.end.character;
				if (!isNaN(range.start.character) && range.start.line === range.end.line) {
					endCharacter -= range.start.character;
				}
				ans[ans.length - 1] = ans[ans.length - 1].substr(0, endCharacter);
			}
			return ans.join("\n");
		}
	}
	return txt;
}

import * as crypto from 'crypto';
import { execSync } from 'child_process';

export function get_fresh_id(): string {
	// This may be overkill, a simple call to random is probably good enough.
	return crypto.createHash('sha256').update(Math.random().toString(36)).digest('hex');
}

export function fname2desc (fname: string): { fileName: string, fileExtension: string, contextFolder: string } {
	const fileName: string = getFileName(fname);
	const fileExtension: string = getFileExtension(fname);
	const contextFolder: string = getContextFolder(fname);
	return { fileName, fileExtension, contextFolder };
}

export function desc2fname (desc: { fileName: string, fileExtension: string, contextFolder: string }): string {
	return path.join(desc.contextFolder, `${desc.fileName}${desc.fileExtension}`);
}

export function getOs (): string {
	if (process.platform === 'linux' || process.platform === 'freebsd' || process.platform === 'openbsd' || process.platform === 'sunos' || process.platform === 'aix') {
		return 'Linux';
	} else if (process.platform === 'darwin') {
		return 'MacOSX';
	}
	return process.platform;
}
