These are the files necessary to produce my personal [blog](http://www.austinrochford.com) using [Hakyll](http://jaspervdj.be/hakyll/), a static site generator written in Haskell.

I recommend building this project inside a Docker container using the following commands.

```bash
stack docker pull
stack build
```

Be prepared for it the dependencies to take a while to build the first time.  To rebuild the site, use

```bash
stack exec blog rebuild
```

which will place its results in the `_site` folder.  To run a local server to preview the site and automatically rebuild it when any of the source files change, use

```bash
stack exec blog watch -- --host "0.0.0.0"
```

After executing this command, there will be a preview webserver running at `localhost:8000`.

The source files for the site are located in `site-src`.  The executable will look in the `posts/` folder for posts.  Each post should be named `YYYY-MM-DD-short-title-for-url.mkd`.  Consult the Hakyll [tutorials](http://jaspervdj.be/hakyll/tutorials.html) for more informations on how to format posts to contain the correct metadata.

This code is distributed under the [MIT License](http://opensource.org/licenses/MIT).
