// purs copies only this file into output/, so each guide is imported from where
// it sits in src/; build-server.sh has esbuild load .md files as text.
import { marked } from "marked";
import joinAnEsportsTeam from "../../src/TeamTavern/Server/Guide/join-an-esports-team.md";
import makeAnEsportsTeam from "../../src/TeamTavern/Server/Guide/make-an-esports-team.md";
import marvelRivalsChampionshipTeam from "../../src/TeamTavern/Server/Guide/marvel-rivals-championship-team.md";
import rocketLeagueTournaments from "../../src/TeamTavern/Server/Guide/rocket-league-tournaments.md";

export const joinAnEsportsTeamText = joinAnEsportsTeam;

export const makeAnEsportsTeamText = makeAnEsportsTeam;

export const marvelRivalsChampionshipTeamText = marvelRivalsChampionshipTeam;

export const rocketLeagueTournamentsText = rocketLeagueTournaments;

// A heading's id is its text as GitHub makes it, lowercased, punctuation
// dropped and spaces turned to hyphens, so a link can name the section.
const headingId = text => text.toLowerCase().replace(/[^\p{L}\p{N} -]/gu, "").replace(/ /g, "-");

marked.use({
    renderer: {
        heading({ tokens, depth, text }) {
            return `<h${depth} id="${headingId(text)}">${this.parser.parseInline(tokens)}</h${depth}>\n`;
        },
    },
});

export const markdownToHtml = markdown => marked.parse(markdown);
