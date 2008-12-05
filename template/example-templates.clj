(load-file "template.clj")
(refer 'template)

(def *letter*
"Dear <%= winner %>,
  We are pleased to inform you that you have been nominated as
president of this years <%= club %> club.  Great job!  You will 
soon receive a set of important items:

<% (doseq [doc documents] -%>
  * <%= doc %>
<% ) -%>

Thanks for all your hard work.
Sincerely,
<%= from %>")

(def *page*
"<h3>Dear <%= winner %>,<h3>
<p>
We are pleased to inform you that you have been nominated as
president of this years <%= club %> club.  Great job!  You will 
soon receive a set of important items:
</p>

<ul>
<% (doseq [doc documents] -%>
<li><%= doc %></li>
<% ) -%>
</ul>

<p>
Thanks for all your hard work.
Sincerely,
<%= from %>
</p>")


(def winner nil)
(def club nil)
(def documents nil)
(def from nil)

(let [letter (template *letter*)
      page (template *page*)]
  (binding [winner "Joe Bob"
            club   "Winners"
            documents ["winner's check" "waiver" "member list"]
            from   "Winners Club Headquarters"]
    (println (letter))
    (println "\n-----------------------------------------\n")
    (println (page)))

  (println "\n-----------------------------------------\n")

  (binding [winner "Suzy Smith"
            club   "All-star Hacker"
            documents ["hacker computer" "waiver" "secret passwords"]
            from   "hackeR cEntrAL"]
    (println (letter))
    (println "\n-----------------------------------------\n")
    (println (page))))
