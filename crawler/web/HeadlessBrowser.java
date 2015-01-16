package web;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.logging.Logger;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.firefox.FirefoxDriver;

import src.Util;

public class HeadlessBrowser {
	private final static Logger logger = Logger.getLogger(HeadlessBrowser.class.getName());
	private static String TOPSY_URL = "http://topsy.com/trackback?url=";
	private WebDriver browser = null;
	private SimpleDateFormat formatter;
	
	public HeadlessBrowser() {
		// Create a new instance of the Firefox driver
		browser = new FirefoxDriver();
		formatter = new SimpleDateFormat("EEE MMM dd HH:mm:ss ZZZZZ yyyy", Locale.ENGLISH);
	}
	
	public ArrayList<HashMap<String,Object>> searchTweets(String url) throws UnsupportedEncodingException {
		ArrayList<HashMap<String,Object>> tweets = new ArrayList<HashMap<String,Object>>();
		
		String urlToVisit = TOPSY_URL + URLEncoder.encode(url, "utf-8");
		
        browser.get(urlToVisit);
        
        try {       	
        	// Find the tweet counter
	        WebElement element = browser.findElement(By.partialLinkText("All Tweets"));
	        
	        int numTweets = Integer.parseInt(element.findElement(By.tagName("span")).getText());
	        // Print the number of tweets found
	        System.out.println("Number of tweets found: " + numTweets);
	        
	        if (numTweets > 0) {
		        tweets = getTweetIds(tweets);
		        
		        int numPages = numTweets/10;
		        int offset;
		        for (int i = 1; i <= numPages; i++) {
	        		offset = i * 10;
		        	if (offset != 0) {
		        		browser.get(urlToVisit+"&offset="+offset);
		        	}
		        	tweets = getTweetIds(tweets);
		        	Util.pause();
		        }
		        
		        System.out.println("Number of tweet ids collected: " + tweets.size());
	        }
			return tweets;
        } catch(NoSuchElementException e) {
        	System.out.println("No tweets found for " + url);
        } catch (InterruptedException e) {
			e.printStackTrace();
		}
		return tweets;
	}
	
	private ArrayList<HashMap<String,Object>> getTweetIds(ArrayList<HashMap<String,Object>> tweets) {
		
		try {
			List<WebElement> tweetList = browser.findElements(By.className("result-tweet"));
			for(WebElement tweet : tweetList) { 
				tweets.add(extractInfoTweet(tweet));
			}
		} catch (NoSuchElementException e) {
			e.printStackTrace();
		} catch (ParseException e) {
			e.printStackTrace();
		}
		
		return tweets;
	}
	
	private HashMap<String,Object> extractInfoTweet(WebElement tweetElem) 
	throws ParseException {
		HashMap<String,Object> tweetInfo = new HashMap<String,Object>();
		long metrics = 0;
		
		String anchorTweet = tweetElem.findElement(By.className("muted")).getAttribute("href");
		String[] anchorSplited = anchorTweet.split("/");
		tweetInfo.put("id", anchorSplited[anchorSplited.length-1]);
		String screenName = tweetElem.findElement(By.className("media-heading")).findElement(By.tagName("small")).getText();
		tweetInfo.put("author", screenName.substring(1, screenName.length()));
		String tweetTS = tweetElem.findElement(By.className("relative-date")).getAttribute("data-timestamp");
		Date date = new Date ();
		date.setTime((long)Integer.parseInt(tweetTS)*1000);
		tweetInfo.put("datetime", date);
		tweetInfo.put("url", anchorTweet);
		tweetInfo.put("retweets", metrics);
        tweetInfo.put("favorites", metrics);
        tweetInfo.put("replies", 0);
		tweetInfo.put("text", tweetElem.findElement(By.className("media-body")).findElement(By.tagName("div")).getText());
		
		return tweetInfo;
	}
	
	public void close() {
        browser.quit();
	}
}
