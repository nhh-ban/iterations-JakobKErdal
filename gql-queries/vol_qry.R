library(glue)

#Task 4B

vol_qry <- function(id, from, to) {
  
  query <- glue('  {{
    trafficData(trafficRegistrationPointId: "{id}") {{
      volume {{
        byHour(from: "{from}", to: "{to}") {{
          edges {{
            node {{
              from
              to
              total {{
                volumeNumbers {{
                  volume
                }}
              }}
            }}
          }}
        }}
      }}
    }}
  }}')
  
  return(query)
  
}