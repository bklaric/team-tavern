// purs copies only this file into output/, so the query is imported from where
// it sits in src/; bundle.ps1 has esbuild load .sql files as text.
import fits from "../../src/TeamTavern/Server/Feed/Fits.sql";

export const fitsText = fits;
