# shinylaskuri

## Running the app in a Docker container
```sh
# Build the image (this will take a while the first time):
docker build . -t shinylaskuri

# Run the image:
docker run --rm -p 3838:3838 shinylaskuri

# Alternatively run the image with the current host directory (${PWD}) mounted:
# This is useful for app development as changes made to server.R and ui.R
# take effect in the running docker container without rebuilding the image.
docker run --rm -p 3838:3838 -v ${PWD}:/srv/shiny-server/ shinylaskuri 
```
The shiny app should now be available at http://localhost:3838/
