import fetch from 'node-fetch';
import * as cheerio from 'cheerio';
import fs from 'fs';

/*

Scrape _all_ news release URLs:

1. Load existing URLs from disk.
2. Start on page 0 of pm.gc.ca news releases. If there’s a 404, quit and save the URLs.
3. Compare URLs scraped to URLs from disk. If there’s a match, go to last step.
4. If all the URLs scraped are new, go to step 2, using page n+1 of pm.gc.ca news releases.
5. Save expanded list of URLs to disk.

*/

const savedNewsReleaseUrls = JSON.parse(fs.readFileSync('data/source/urls-news-releases.json'));

// console.log(await scrapeUrlsFromNewsPage(833))

function areAllUrlsInList(urlsToCheck, listToCheck) {
    urlsToCheck.every((url) => listToCheck.includes(url))
}

console.log(extractUrlsFromNewsPage(await scrapeNewsPage(0)));

async function scrapeNewsPage(pageNumber) {
    return await fetchNewsReleaseHtmlJSON(pageNumber);
}

async function fetchNewsReleaseHtmlJSON(page) {
    const response = await fetch("https://pm.gc.ca/views/ajax", {
        "headers": {
            "accept": "*/*",
            "accept-language": "en-US,en;q=0.9",
            "content-type": "application/x-www-form-urlencoded; charset=UTF-8",
            // "sec-ch-ua": "\" Not A;Brand\";v=\"99\", \"Chromium\";v=\"98\", \"Google Chrome\";v=\"98\"",
            // "sec-ch-ua-mobile": "?0",
            // "sec-ch-ua-platform": "\"macOS\"",
            // "sec-fetch-dest": "empty",
            // "sec-fetch-mode": "cors",
            // "sec-fetch-site": "same-origin",
            "x-requested-with": "XMLHttpRequest",
            // "Referer": "https://pm.gc.ca/en/news/releases?page=1",
            // "Referrer-Policy": "strict-origin-when-cross-origin"
        },
        "body": `view_name=news&view_display_id=page_1&view_args=1&page=${page}`,
        "method": "POST"
    });
    const responseJSON = await response.json();

    return responseJSON;
}

function domifyNewsPage(newsPageHtmlJSON) {
    return cheerio.load(newsPageHtmlJSON.filter((contentItem) => {
        return contentItem.command == "insert" && contentItem.selector == ".js-view-dom-id-";
    }).pop()['data']);
}

function extractUrlsFromNewsPage(newsPageHtmlJSON) {
    const newsReleaseListingHtml = domifyNewsPage(newsPageHtmlJSON);
    
    const scrapedUrls =
        [...new Set(newsReleaseListingHtml('a')
            .toArray()
            .map((linkElement) => newsReleaseListingHtml(linkElement).attr('href'))
            .filter((link) => link.startsWith('/en/news/')))];
    
    // To pull just "chnage in the ranks" URLs (which always include "change" and "rank")
    // .filter((link) => link.includes('change') && link.includes('rank')))
    // NB! Some include "reappointment"

    return scrapedUrls;
}

