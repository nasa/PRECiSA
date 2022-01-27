/**
 * vscode-pvs colors.
 */

// based on the 256 color scheme, see colors at https://misc.flogisoft.com/bash/tip_colors_and_formatting
export enum PvsColor {
	blue = "blue", 
	darkblue = "darkblue",
	yellow = "yellow",
	darkyellow = "darkyellow",
	gold = "gold",
	green = "green", 
	darkgreen = "darkgreen", 
	red = "red",
	white = "white",
	black = "black",
	gray = "gray",
	darkgray = "darkgray"
};

export declare type XTermColorTheme = "dark" | "light";

export const ANSI_ESC: string = "\x1b[";
export const ANSI_FOREGROUND: string = "38;";
export const ANSI_BACKGROUND: string = "48;";
export const ANSI_RESET_FOREGROUND: string = "38;39;";
export const ANSI_RESET_BACKGROUND: string = "48;49;";
export const ANSI_RESET: string = `${ANSI_ESC}0m`;

/**
 * Utility function, returns a color based on the theme
 */
export function getColor (color: PvsColor, theme: XTermColorTheme): PvsColor {
	switch (color) {
		case PvsColor.yellow: {
			return theme === "dark" ? PvsColor.yellow : PvsColor.darkyellow;
		}
		case PvsColor.blue: {
			return theme === "dark" ? PvsColor.blue : PvsColor.darkblue;
		}
		case PvsColor.green: {
			return theme === "dark" ? PvsColor.green : PvsColor.darkgreen;
		}
		case PvsColor.gray: {
			return theme === "dark" ? PvsColor.gray : PvsColor.black;
		}
		default: {
			break;
		}
	}
	return color;
}

export const htmlColorCode: { [ key in PvsColor ]: string } = {
	blue: "#00b6fc",
	darkblue: "#117AD0", // navy blue
	green: "mediumspringgreen",
	darkgreen: "darkgreen",
	yellow: "yellow",
	darkyellow: "#999900",
	gold: "gold",
	red: "#f26158",
	black: "#1e1e1e",
	white: "whitesmoke",
	gray: "gray",
	darkgray: "darkgray"
};

// export const colorMap: { [ key in vscodeColor ]: string } = {
// 	blue: "#00adf4",
// 	yellow: "yellow",
// 	green: "#55b92d",
// 	red: "#f26158"
// }

export const ansiColorCode: { [ key in PvsColor ]: number } = {
	blue: 39, // deepskyblue
	darkblue: 26, 
	yellow: 229, // bright yellow 
	darkyellow: 202, // orange-red
	gold: 220, // gold
	green: 42, // springgreen
	darkgreen: 22,
	red: 160,
	white: 254, // whitesmoke
	black: 0,
	gray: 7, // silver
	darkgray: 239
};

const ansiColor256: { [color in PvsColor]: string } = {
    yellow: `5;${ansiColorCode.yellow}`,
    darkyellow: `5;${ansiColorCode.darkyellow}`,
	gold: `5;${ansiColorCode.gold}`,
    blue: `5;${ansiColorCode.blue}`,
    darkblue: `5;${ansiColorCode.darkblue}`,
    red: `5;${ansiColorCode.red}`,
    white: `5;${ansiColorCode.white}`,
    black: `5;${ansiColorCode.black}`,
    green: `5;${ansiColorCode.green}`,
	darkgreen: `5;${ansiColorCode.darkgreen}`,
	gray: `5;${ansiColorCode.gray}`,
	darkgray: `5;${ansiColorCode.darkgray}`,
};

export const ansiColorRegex: RegExp = /\x1b\[(\d;?)*m/g;

/**
 * Utility function, removes ansi sequences from the text
 */
export function getPlainText (ctext: string): string {
	if (ctext) {
		return ctext.replace(new RegExp(ansiColorRegex), "");
	}
	return ctext;
}

/**
 * Utility function, checks if the provided text is plain text (i.e., without ansi escape sequences)
 */
export function isPlainText (ctext: string): boolean {
	if (ctext) {
		return !(new RegExp(ansiColorRegex).test(ctext));
	}
	return true;
}

export interface AnsiHighlightOptions {
	background?: PvsColor, 
	foreground?: PvsColor,
	bold?: boolean
}
export function ansiColorText (text: string, opt?: AnsiHighlightOptions): string {
	if (text) {
		opt = opt || {};
		let ansiCode: string = "";
		ansiCode += opt.bold ? `${ANSI_ESC}1m` : "";
		ansiCode += opt.background && ansiColor256[opt.background] ?
			`${ANSI_ESC}${ANSI_BACKGROUND}${ansiColor256[opt.background]}m`
				: "";
		ansiCode += opt.foreground && ansiColor256[opt.foreground] ?
			`${ANSI_ESC}${ANSI_FOREGROUND}${ansiColor256[opt.foreground]}m`
				: "";
		ansiCode += text.toLocaleString();
		ansiCode += ANSI_RESET; // reset attributes for the following text
		return ansiCode;
	}
	return text;
}
export function colorText(text: string, opt: PvsColor | AnsiHighlightOptions): string {
	if (text) {
		if (typeof opt === "object") {
			return ansiColorText(text, opt);
		} else {
			return ansiColorText(text, { foreground: opt });
		}	
		//`\x1b[38;5;${ansiColorCode[color]}m${text.toLocaleString()}\x1b[0m`; // \x1b[0m resets all attributes -- ANSI encoding
	}
	return text;
}
