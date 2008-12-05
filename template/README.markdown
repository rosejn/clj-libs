## Template: 

A library for creating templates to generate HTML pages, email messages,
configuration files etc.  You can embed Clojure expressions inside tags in a
document, and then instantiate the template under different bindings.

The library is fine for use right now, but additional bells and whistles are
planned for release shortly.  These include:

 * Support for passing in a hash or a vector of hashes which will be used for
   bindings.
 * More options for dealing with line trimming and white-space handling
 * Line caching for faster template instantiation

Please see the examples in example-templates.clj for usage under the current
API. 
