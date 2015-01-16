package api;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import oauth.signpost.OAuthConsumer;
import oauth.signpost.commonshttp.CommonsHttpOAuthConsumer;
import oauth.signpost.exception.OAuthCommunicationException;
import oauth.signpost.exception.OAuthExpectationFailedException;
import oauth.signpost.exception.OAuthMessageSignerException;

import org.apache.http.Header;
import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.ResponseHandler;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.BasicResponseHandler;
import org.apache.http.impl.client.HttpClients;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import src.Util;
import web.TweetRepliesReader;

public class TwitterApp {
	private final static Logger logger = Logger.getLogger(TwitterApp.class.getName());
	private String ACCESSTOKEN = "";
	private String ACCESSSECRET = "";
	private String APIKEY = "";
	private String APISECRET= "";
	protected OAuthConsumer consumer = null;
	protected Integer remainingRequests;
	protected Long limitReset;
	protected JSONParser parser = null;
	protected static Integer API_LIMIT = 180;
	private static Integer MAX_ATTEMPTS = 3;
	private static String TWRTCOUNTER = "retweet_count";
	private static String TWFVCOUNTER = "favorite_count";
	private static String TWTXT = "text";
	private static String TWDATE = "created_at";
	private static String TWAUTHOR = "user";
	private static String TWAUTHORHANDLE = "screen_name";
	private static String TWID = "id_str";
	private static String TWRP = "in_reply_to_status_id_str";
	private static String TWRT = "retweeted_status";
	private static String TWSOURCE = "source";
	private static String TWITTERURL = "http://twitter.com/";
	private TweetRepliesReader twRepReader = null;
	
	
	public TwitterApp() {
		setUp();
		consumer = new CommonsHttpOAuthConsumer(APIKEY,APISECRET);
		consumer.setTokenWithSecret(ACCESSTOKEN, ACCESSSECRET);
		remainingRequests = API_LIMIT;
		parser = new JSONParser();
		twRepReader = new TweetRepliesReader();
	}
	
	private void setUp() {
		try {
			BufferedReader buffReader = new BufferedReader(new FileReader("conf"));
			String currentLine = buffReader.readLine(); //Discarding the first line since it contains indications
			while ((currentLine = buffReader.readLine()) != null) {
				if (currentLine.indexOf("TWACCESSTOKEN") != -1) {
					ACCESSTOKEN = currentLine.split("=")[1];
				} else if (currentLine.indexOf("TWACCESSSECRET") != -1) {
					ACCESSSECRET = currentLine.split("=")[1];
				} else if(currentLine.indexOf("TWAPIKEY") != -1) {
					APIKEY = currentLine.split("=")[1];
				} else if(currentLine.indexOf("TWAPISECRET") != -1) {
					APISECRET = currentLine.split("=")[1];
				}
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	protected HttpResponse pause(HttpGet httpGet) 
	throws InterruptedException, ClientProtocolException, IOException {
		HttpResponse response = null;
		
		long now = System.currentTimeMillis();
		long waitingTime = limitReset-now;
		Util.printMessage("API request limit reached, we have to wait " + 
			    		  (waitingTime/60000) + " minutes for the next time window", 
			    		  "info", logger);
		if (waitingTime > 0) {
			Thread.sleep(waitingTime);
			response = doRequest(httpGet);
			while (remainingRequests == 0) {
				System.out.println("Still banned.");
				Thread.sleep(60000);
				response = doRequest(httpGet);
			}
		}
		else {
			response = doRequest(httpGet);
		}
		
		return response;
	}
	
	protected HttpResponse doRequest(HttpGet httpGet) 
	throws ClientProtocolException, IOException {
		HttpClient client = HttpClients.createDefault();
		HttpResponse response = client.execute(httpGet);
		setRemainingRequests(response);
		return response;
	}
	
	protected void setRemainingRequests(HttpResponse response) 
	throws ClientProtocolException, IOException 
	{	
        Header[] headers = response.getAllHeaders();
        for(Header header:headers) {
        	if (header.getName().equalsIgnoreCase("x-rate-limit-remaining"))
        		remainingRequests = Integer.parseInt(header.getValue());
        	if (header.getName().equalsIgnoreCase("x-rate-limit-reset")) {
        		long timeLimitMS = Long.parseLong(header.getValue())*1000;
        		limitReset = timeLimitMS;
        	}
    	}
	}
	
	protected HttpResponse request(String url) 
	throws OAuthMessageSignerException, OAuthExpectationFailedException, 
		   OAuthCommunicationException, ClientProtocolException, InterruptedException, 
		   IOException 
	{
		int attemptCounter = 0;
		
		HttpGet httpGet = new HttpGet(url);
		consumer.sign(httpGet);
		
		HttpResponse response = null;
		if (remainingRequests == 0) {
			response = pause(httpGet);
		}
		else {
			response = doRequest(httpGet);
		}
		
		while (response.getStatusLine().getStatusCode() != 200 && attemptCounter < MAX_ATTEMPTS) {
			if (response.getStatusLine().getStatusCode() == 401 ||
	        	response.getStatusLine().getStatusCode() == 406) {
	        	Util.printMessage("Ingnoring invalid URL: " + url, "severe",logger);
	        	return null;
	        }
			else {
                attemptCounter += 1;
				Util.printMessage("Wrong Twitter API response code, got: " + 
						  		  response.getStatusLine().getStatusCode() + 
						  		  " expected 200","info",logger);
				Thread.sleep(10000); //Wait for 10 seconds and try again
				response = doRequest(httpGet);
			}
		}
		
		return response;
	}
	
	protected JSONObject getStatus(String idStatus) 
	throws OAuthMessageSignerException, OAuthExpectationFailedException, 
		   OAuthCommunicationException, ClientProtocolException, 
		   InterruptedException, IOException, ParseException 
	{
		String fullURL = "https://api.twitter.com/1.1/statuses/show/"+idStatus+".json";
        HttpResponse response = null;
		
		Util.printMessage("Remaining requests  " + remainingRequests + " in this time window","info",logger);
		Util.printMessage("Getting information about tweet: " + idStatus,"info",logger);
		
		response = request(fullURL);
		
        if (response != null && response.getStatusLine().getStatusCode() == 200) {	
            ResponseHandler<String> handler = new BasicResponseHandler();
            String body = handler.handleResponse(response);
            Object obj = parser.parse(body);
            JSONObject status = (JSONObject) obj;
            return status;
        }
        else {
        	Util.printMessage("Couldn't find the tweet related to: " + fullURL,"info",logger);
        	return null;
        }
	}
	
	protected HashMap<String,Object> extractTweet(JSONObject status) 
	throws Exception {
		HashMap<String,Object> tweet = new HashMap<String,Object>();
		SimpleDateFormat formatter = new SimpleDateFormat("EEE MMM dd HH:mm:ss ZZZZZ yyyy", Locale.ENGLISH);
		
		String idTweet = (String) status.get(TWID);
		tweet.put("retweets", status.get(TWRTCOUNTER));
        tweet.put("favorites", status.get(TWFVCOUNTER));
        Date tweetDateTime = formatter.parse((String) status.get(TWDATE));
        tweet.put("datetime", tweetDateTime);
        tweet.put("text", status.get(TWTXT));
        JSONObject authorObj = (JSONObject) status.get(TWAUTHOR);
        String author = (String) authorObj.get(TWAUTHORHANDLE);
        tweet.put("author", author);
        String statusURL = TWITTERURL+author+"/status/"+status.get(TWID);
        tweet.put("id", idTweet);
        tweet.put("url", statusURL);
        String source = (String) status.get(TWSOURCE);
        Pattern replace = Pattern.compile("(<[a|A][^>]*>|</[a|A]>)");
        Matcher matcher = replace.matcher(source);
        tweet.put("source", (String) matcher.replaceAll(""));
        
        if (status.containsKey(TWRT)) {  // Is a RT
        	JSONObject originalTweet = (JSONObject) status.get(TWRT);
        	tweet.put("id_retweet", (String) originalTweet.get(TWID));
        	tweet.put("replies", 0);
        }
        else {
        	tweet.put("id_retweet", null);
        	ArrayList<String> idReplies = twRepReader.getIdReplies(statusURL);
        	int numReplies = idReplies.size();
        	/*for (String idReply : idReplies) {
        		if (!isReply(idTweet,idReply))
        			numReplies -= 1;
        	}*/
        	tweet.put("replies", numReplies);
        }
        
        return tweet;
	}
			
	private boolean isReply(String idTweetOrg, String idReplyTweet) 
	throws OAuthMessageSignerException, OAuthExpectationFailedException, 
		   OAuthCommunicationException, ClientProtocolException, 
		   InterruptedException, IOException, org.json.simple.parser.ParseException {
		JSONObject status = getStatus(idReplyTweet);
		String idTweetReplied = (String) status.get(TWRP);
		return idTweetReplied.equals(idTweetOrg);
	}
}
