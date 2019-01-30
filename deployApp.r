
# --- upload to shinyapps.io --------------------------------------------------

# you need to have an account on shinyapps.io
# In your account you find the token or can create a new one.

# install.packages("rsconnect")

rsconnect::setAccountInfo(
  name='rluech',
  token='xxx', 
  secret='xxx'
  )

rsconnect::deployApp(
  '~/shiny/jahresbericht',
  account = 'rluech'
)

# Link
# https://rluech.shinyapps.io/jahresbericht