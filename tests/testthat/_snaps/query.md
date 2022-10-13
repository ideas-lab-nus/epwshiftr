# ESGF Query Parameter works

    [1] "x=false"

---

    [1] "x!=1"

---

    [1] "x!=solr%2Bjson"

---

    [1] "x = true"

---

    [1] "x = 1"

---

    [1] "x = solr%2Bjson"

---

    x = false

---

    x != 1

---

    x != solr+json

# EsgfQuery$print()

    Code
      EsgfQuery$new()$params(table_id = "Amon", member_id = "r1i1p1f1")$print()
    Message <cliMessage>
      == ESGF Query ==================================================================
      * Host: https://esgf-node.llnl.gov/esg-search
      * Facet cache built at: 2022-10-13 23:27:17
      
      -- <QUERY PARAMETER> -----------------------------------------------------------
      * format = application/solr+json
      * limit = 10
      * distrib = true
      * offset = 0
      * type = Dataset
      * latest = true
      * fields = *
      * project = CMIP6
      * table_id = Amon
      * member_id = r1i1p1f1

