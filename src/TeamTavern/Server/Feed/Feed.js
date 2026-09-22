// purs copies only this file into output/, so the query is imported from where
// it sits in src/; build-server.sh has esbuild load .sql files as text.
import feed from "../../src/TeamTavern/Server/Feed/Feed.sql";

export const feedText = feed;
