# ESGF Query Parameter works

    Code
      format(new_query_param("x", list(value = TRUE, negate = TRUE)))
    Output
      [1] "x=false"

---

    Code
      format(new_query_param("x", list(value = 1, negate = TRUE)))
    Output
      [1] "x!=1"

---

    Code
      format(new_query_param("x", list(value = "solr+json", negate = TRUE)))
    Output
      [1] "x!=solr%2Bjson"

---

    Code
      format(new_query_param("x", TRUE), space = TRUE)
    Output
      [1] "x = true"

---

    Code
      format(new_query_param("x", 1), space = TRUE)
    Output
      [1] "x = 1"

---

    Code
      format(new_query_param("x", "solr+json"), space = TRUE)
    Output
      [1] "x = solr%2Bjson"

---

    Code
      print(new_query_param("x", list(value = TRUE, negate = TRUE)))
    Output
      x = false

---

    Code
      print(new_query_param("x", list(value = 1, negate = TRUE)))
    Output
      x != 1

---

    Code
      print(new_query_param("x", list(value = "solr+json", negate = TRUE)))
    Output
      x != solr+json

# EsgfQuery$print()

    Code
      EsgfQuery$new("a", listing = FALSE)$params(table_id = "Amon", member_id = "r1i1p1f1")$
        print()
    Message <cliMessage>
      == ESGF Query ==================================================================
      * Host: a
      * Facet listing built at: <NONE>
        NOTE: You may run `$build_listing()` to create the listing
      * Last query sent at: <NONE>
      
      -- <Query Parameter> -----------------------------------------------------------
      * project = CMIP6
      * fields = *
      * latest = true
      * type = Dataset
      * offset = 0
      * distrib = true
      * limit = 10
      * format = application/solr+json
      * table_id = Amon
      * member_id = r1i1p1f1

---

    Code
      EsgfQuery$new(host, listing = FALSE)$params(table_id = "Amon", member_id = "r1i1p1f1")$
        print()
    Message <cliMessage>
      == ESGF Query ==================================================================
      * Host: https://esgf.ceda.ac.uk/esg-search
      * Facet listing built at: <NONE>
        NOTE: You may run `$build_listing()` to create the listing
      * Last query sent at: <NONE>
      
      -- <Query Parameter> -----------------------------------------------------------
      * project = CMIP6
      * fields = *
      * latest = true
      * type = Dataset
      * offset = 0
      * distrib = true
      * limit = 10
      * format = application/solr+json
      * table_id = Amon
      * member_id = r1i1p1f1

