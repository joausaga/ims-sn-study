package api;

import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Logger;

import org.apache.commons.validator.routines.UrlValidator;
import org.apache.http.HttpResponse;
import org.apache.http.client.ResponseHandler;
import org.apache.http.impl.client.BasicResponseHandler;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import src.Util;

public class TweetSearch extends TwitterApp {
	private final static Logger logger = Logger.getLogger(TweetSearch.class.getName());
	private static String REQUESTURL = "https://api.twitter.com/1.1/search/tweets.json?q=";
	private static String ARRAYTWS = "statuses";
    private static Integer MAX_ATTEMPTS = 5;
	
	
	public TweetSearch() {
		super();
	}
	
	public ArrayList<HashMap<String,Object>> getTweets(String url) 
	throws Exception 
	{
		HttpResponse response = null;
		ArrayList<HashMap<String,Object>> tweets = new ArrayList<HashMap<String,Object>>();
		
        String fullURL = REQUESTURL+URLEncoder.encode(url, "utf-8");
		
		Util.printMessage("Remaining requests  " + remainingRequests + " in this time window","info",logger);
		Util.printMessage("Searching tweets for: " + url,"info",logger);	

		UrlValidator urlValidator = new UrlValidator();
		if (urlValidator.isValid(url)) {
			response = request(fullURL);
            if (response != null && response.getStatusLine().getStatusCode() == 200) {
                ResponseHandler<String> handler = new BasicResponseHandler();
                String body = handler.handleResponse(response);
                
                Object obj = parser.parse(body);
                JSONObject jsonObj = (JSONObject) obj;
                JSONArray statuses = (JSONArray) jsonObj.get(ARRAYTWS);
                Iterator<JSONObject> iterator = statuses.iterator();
                while (iterator.hasNext()) {
                    JSONObject status = (JSONObject) iterator.next();
                    HashMap<String,Object> tweet = extractTweet(status);
                    tweets.add(tweet);
                }
                if (tweets.size() > 0) {
                    Util.printMessage("Found " + tweets.size() + " tweets related to the community.","info",logger);
                }
            }
            else {
                Util.printMessage("Couldn't find the tweet related to: " + fullURL,"info",logger);
            }
		}
        else {
        	Util.printMessage("Ingnoring invalid URL: " + fullURL, "severe",logger);
        }
		
		return tweets;
	}
	
	public HashMap<String,Object> getTweet(String idTweet) 
	throws Exception 
	{
		HashMap<String,Object> tweet = new HashMap<String,Object>();
		
		JSONObject status = getStatus(idTweet);
		if (status != null)
			tweet = extractTweet(status);
		
        return tweet;
	}
}
