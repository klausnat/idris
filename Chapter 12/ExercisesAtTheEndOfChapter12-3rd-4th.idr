-- 3. You could define record for representing an article on a social website...

record Votes where
       constructor MkVotes
       upvotes : Integer
       downvotes : Integer
       
record Article where
       constructor MkArticle
       title : String
       url : String
       score : Votes
       
initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

-- write a function to calculate an overall score of a given article where the score = upwotes - downvotes

getScore : Article -> Integer
getScore article = upvotes (score article) - downvotes (score article)

badSite : Article
badSite = MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 47)


goodSite : Article
goodSite = MkArticle "Good Page" "http://example.com/good" (MkVotes 101 7)

-- 4. Write addUpVote and addDownvote functions that modify an articles score up or down.
addUpvote : Article -> Article
addUpvote article = record {score -> upvotes $= (+1)} article

addDownvote : Article -> Article
addDownvote article = record {score -> downvotes $= (+1)} article
