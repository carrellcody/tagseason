library(pdftools)
library(tidyverse)
library(data.table)
library(stringr)
library(tidyverse)
library(dplyr)
library(readr)



drawrecapdeer25<-"Raw data/draw_recap_reports/2025_Deer_DR.pdf"
drawrecapelk25<-"Raw data/draw_recap_reports/2025_Elk_DR.pdf"
drawrecapant25<-"Raw data/draw_recap_reports/2025_Ant_DR.pdf"
brochure2025<-pdf_text("Raw data/biggamebrochures/2025BGB.pdf")
brochure2026<-pdf_text("Raw data/biggamebrochures/2026BGB.pdf")


text<-paste(brochure2025, collapse = "\n")
text26<-paste(brochure2026, collapse = "\n")
writeLines(text, "Raw data/biggamebrochures/2025FullText.txt")
writeLines(text26, "Raw data/biggamebrochures/2026FullText.txt")


#draw recap mapping page numbers

drawrecapdeer25_text<-pdf_text(drawrecapdeer25)
drawrecapelk25_text<-pdf_text(drawrecapelk25)
drawrecapant25_text<-pdf_text(drawrecapant25)


# Create data frame with page numbers----
#for deer----
 hunt_codes <- lapply(seq_along(drawrecapdeer25_text), 
                      function(i) {
  page_text <- drawrecapdeer25_text[[i]]
  
  codes <- str_extract_all(page_text, "[A-Z]{2}[0-9]{3}[A-Z][0-9][A-Z]")[[1]]
  
  if (length(codes) > 0) {
    data.frame(
      HuntCode = codes,
      Page = i,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
})

deer25code_pages <- as.data.table(bind_rows(hunt_codes)) %>%
  distinct() %>%   # remove duplicates
  arrange(HuntCode, Page)
write_csv(deer25code_pages, "deer25code_pages.csv")

#for elk----
elkhunt_codes <- lapply(seq_along(drawrecapelk25_text), 
                     function(i) {
                       page_text <- drawrecapelk25_text[[i]]
                       
                       codes <- str_extract_all(page_text, "[A-Z]{2}[0-9]{3}[A-Z][0-9][A-Z]")[[1]]
                       
                       if (length(codes) > 0) {
                         data.frame(
                           HuntCode = codes,
                           Page = i,
                           stringsAsFactors = FALSE
                         )
                       } else {
                         NULL
                       }
                     })

elk25code_pages <- as.data.table(bind_rows(elkhunt_codes)) %>%
  distinct() %>%   # remove duplicates
  arrange(HuntCode, Page)
write_csv(elk25code_pages, "elk25code_pages.csv")

#for antelope----
anthunt_codes <- lapply(seq_along(drawrecapant25_text), 
                        function(i) {
                          page_text <- drawrecapant25_text[[i]]
                          
                          codes <- str_extract_all(page_text, "[A-Z]{2}[0-9]{3}[A-Z][0-9][A-Z]")[[1]]
                          
                          if (length(codes) > 0) {
                            data.frame(
                              HuntCode = codes,
                              Page = i,
                              stringsAsFactors = FALSE
                            )
                          } else {
                            NULL
                          }
                        })

ant25code_pages <- as.data.table(bind_rows(anthunt_codes)) %>%
  distinct() %>%   # remove duplicates
  arrange(HuntCode, Page)
write_csv(ant25code_pages, "ant25code_pages.csv")



# Define the regex pattern----
deerpattern <- "D-\\w-\\d{3}-\\w\\d-\\w"
elkpattern<- "E-\\w-\\d{3}-\\w\\d-\\w"
antelopepattern<- "A-\\w-\\d{3}-\\w\\d-\\w"
moosepattern<- "M-\\w-\\d{3}-\\w\\d-\\w"
bearpattern<- "B-\\w-\\d{3}-\\w\\d-\\w"



# Extract all matches into a list

allcodes25<-list(
deermatches = sort(unique(unlist(regmatches(brochure2025, gregexpr(pattern=deerpattern, text=brochure2025, perl = TRUE))))),
elkmatches = unique(unlist(regmatches(brochure2025, gregexpr(pattern=elkpattern, text=brochure2025, perl = TRUE)))),
antelopematches = unique(unlist(regmatches(brochure2025, gregexpr(pattern=antelopepattern, text=brochure2025, perl = TRUE)))),
moosematches = unique(unlist(regmatches(brochure2025, gregexpr(pattern=moosepattern, text=brochure2025, perl = TRUE)))),
bearmatches = unique(unlist(regmatches(brochure2025, gregexpr(pattern=bearpattern, text=brochure2025, perl = TRUE))))
)

allcodes26<-list(
  deermatches = sort(unique(unlist(regmatches(brochure2026, gregexpr(pattern=deerpattern, text=brochure2026, perl = TRUE))))),
  elkmatches = unique(unlist(regmatches(brochure2026, gregexpr(pattern=elkpattern, text=brochure2026, perl = TRUE)))),
  antelopematches = unique(unlist(regmatches(brochure2026, gregexpr(pattern=antelopepattern, text=brochure2026, perl = TRUE)))),
  moosematches = unique(unlist(regmatches(brochure2026, gregexpr(pattern=moosepattern, text=brochure2026, perl = TRUE)))),
  bearmatches = unique(unlist(regmatches(brochure2026, gregexpr(pattern=bearpattern, text=brochure2026, perl = TRUE))))
)


#pads the empty cells with "" so that it can properly form a data table where all columns are the same length
max_len <- max(sapply(allcodes25, length))
allcodes_padded25 <- lapply(allcodes25, function(x) {
  c(x, rep("", max_len - length(x)))  # pad with empty strings
})

max_len <- max(sapply(allcodes26, length))
allcodes_padded26 <- lapply(allcodes26, function(x) {
  c(x, rep("", max_len - length(x)))  # pad with empty strings
})


#creates data table with all padded lists. do.call makes each item in each list an individual row
final25<-as.data.table(do.call(cbind, allcodes_padded25))
final26<-as.data.table(do.call(cbind, allcodes_padded26))


#writes to a csv file
write.csv(final25, "Raw data/biggamebrochures/2025HuntCodes.csv", row.names = FALSE)
write.csv(final26, "Raw data/biggamebrochures/2026HuntCodes.csv", row.names = FALSE)

#Find all the differences between the two brochures
compare_columns <- function(final25, final26) {
  cols <- intersect(names(final25), names(final26))
  
  rbindlist(lapply(cols, function(col) {
    rbind(
      data.table(column = col,
                 table = "final25_missing",
                 value = setdiff(final26[[col]], final25[[col]])),
      data.table(column = col,
                 table = "final26_missing",
                 value = setdiff(final25[[col]], final26[[col]]))
    )
  }))
}

diffs <- compare_columns(final25, final26)

write.csv(diffs,"Raw data/biggamebrochures/25v26.csv")
