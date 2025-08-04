# Use an R base image
FROM rocker/r-ver:4.3.1

# Set the working directory inside the container
WORKDIR /app

# Copy your R app files into the container
COPY . .

# Install any R packages your app needs
RUN R -e "install.packages(c('shiny', 'shinyFeedback'), repos='https://cloud.r-project.org/')"

# Expose the port your app will run on
EXPOSE 8080

# Command to run your app
CMD ["R", "-e", "shiny::runApp('/app')"]
