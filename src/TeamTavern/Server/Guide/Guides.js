// purs copies only this file into output/, so each guide is imported from where
// it sits in src/; bundle.ps1 has esbuild load .md files as text.
import { marked } from "marked";
import deadlockRankedWithFriends from "../../src/TeamTavern/Server/Guide/deadlock-ranked-with-friends.md";
import fncsTeam from "../../src/TeamTavern/Server/Guide/fncs-team.md";
import joinAnEsportsTeam from "../../src/TeamTavern/Server/Guide/join-an-esports-team.md";
import makeAnEsportsTeam from "../../src/TeamTavern/Server/Guide/make-an-esports-team.md";
import marvelRivalsChampionshipTeam from "../../src/TeamTavern/Server/Guide/marvel-rivals-championship-team.md";
import marvelRivalsRankedWithFriends from "../../src/TeamTavern/Server/Guide/marvel-rivals-ranked-with-friends.md";
import overwatchRankedWithFriends from "../../src/TeamTavern/Server/Guide/overwatch-ranked-with-friends.md";
import rainbowSixSiegeRankedWithFriends from "../../src/TeamTavern/Server/Guide/rainbow-six-siege-ranked-with-friends.md";
import rocketLeagueTournaments from "../../src/TeamTavern/Server/Guide/rocket-league-tournaments.md";
import theFinalsRankedWithFriends from "../../src/TeamTavern/Server/Guide/the-finals-ranked-with-friends.md";
import valorantRankedWithFriends from "../../src/TeamTavern/Server/Guide/valorant-ranked-with-friends.md";

export const deadlockRankedWithFriendsText = deadlockRankedWithFriends;

export const fncsTeamText = fncsTeam;

export const joinAnEsportsTeamText = joinAnEsportsTeam;

export const makeAnEsportsTeamText = makeAnEsportsTeam;

export const marvelRivalsChampionshipTeamText = marvelRivalsChampionshipTeam;

export const marvelRivalsRankedWithFriendsText = marvelRivalsRankedWithFriends;

export const overwatchRankedWithFriendsText = overwatchRankedWithFriends;

export const rainbowSixSiegeRankedWithFriendsText = rainbowSixSiegeRankedWithFriends;

export const rocketLeagueTournamentsText = rocketLeagueTournaments;

export const theFinalsRankedWithFriendsText = theFinalsRankedWithFriends;

export const valorantRankedWithFriendsText = valorantRankedWithFriends;

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
