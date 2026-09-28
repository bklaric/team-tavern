// purs copies only this file into output/, so each guide is imported from where
// it sits in src/; build-server.sh has esbuild load .md files as text.
import { marked } from "marked";
import joinAnEsportsTeam from "../../src/TeamTavern/Server/Guide/join-an-esports-team.md";

export const joinAnEsportsTeamText = joinAnEsportsTeam;

export const markdownToHtml = markdown => marked.parse(markdown);
