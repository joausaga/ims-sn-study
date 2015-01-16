package api;

import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import src.Util;


public class TweetUpdater extends TwitterApp {
	private final static Logger logger = Logger.getLogger(TweetSearch.class.getName());
	
	
	public TweetUpdater() {
		super();
	}
	
	public HashMap<String,String> getExpandedURL(String idTweet) 
	throws Exception 
	{
		HashMap<String,String> data = new HashMap<String,String>();
		String source;
		JSONObject tweet = getStatus(idTweet);
		
		if (tweet != null) {
	        source = (String) tweet.get("source");
	        Pattern replace = Pattern.compile("(<[a|A][^>]*>|</[a|A]>)");
	        Matcher matcher = replace.matcher(source);
	        data.put("source", (String) matcher.replaceAll(""));
	        JSONObject entities = (JSONObject) tweet.get("entities");
	        JSONArray urls = (JSONArray) entities.get("urls");
	        Iterator<JSONObject> iterator = urls.iterator();
	        while (iterator.hasNext()) {
	            JSONObject url = (JSONObject) iterator.next();
	            String expandedUrl = (String) url.get("expanded_url");
	            if (expandedUrl.contains("ideascale")) {
	            	data.put("url", expandedUrl);
	            }
	        }
	        if (!data.containsKey("url")) {
	        	Util.printMessage("The tweet: " + idTweet + " does not contain " +
					  			  "an ideascale URL", "info", logger);
	        }
		}
		
		return data;
	}
}
