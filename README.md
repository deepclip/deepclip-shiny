deepclip-shiny
==============

deepclip-shiny is an R shiny web interface for [DeepCLIP](https://github.com/deepclip/deepclip).

## Deploying

Build the Docker image using `docker run`

```
sudo docker build -t deepclip-shiny .
```

Then run the image and bind it to the correct host port (e.g. 3846 in this example)

```
sudo docker run -d -p 3846:3838 --restart always -v /srv/deepclip/data:/srv/deepclip/data deepclip-shiny
```

FASTA and GTF files for reference genomes should be put in `/srv/deepclip/data` on the host file system.
