package web;

import java.util.ArrayList;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import src.Util;

public class TweetRepliesReader extends HTMLReader {
	private static String IDREPLIESLIST = "stream-items-id";
	
	
	public TweetRepliesReader() {
		super();
		prepareUserAgent();
	}
	
	public ArrayList<String> getIdReplies(String twURL) throws Exception  {
		String content = getUrlContent(Util.toURI(twURL));
		Document doc = Jsoup.parse(content);
		ArrayList<String> idReplies = new ArrayList<String>();
    	
		Element repliesList = doc.getElementById(IDREPLIESLIST);
		if (repliesList != null) {
			Elements replies = repliesList.children();
			for (Element reply : replies) {
				idReplies.add(reply.child(0).attr("data-tweet-id"));
			}
		}
		
    	return idReplies;
    }
	
}
