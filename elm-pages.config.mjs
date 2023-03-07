import { defineConfig } from "vite";
import adapter from "./adapter.mjs";

export default {
  vite: defineConfig({}),
  adapter,
  headTagsTemplate(context) {
    return `
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<!-- Import Ionicon icons & Google Fonts our Bootstrap theme relies on -->
<link href="//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css" rel="stylesheet" type="text/css">
<link href="//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic" rel="stylesheet" type="text/css">
<!-- Import the custom Bootstrap 4 theme from our hosted CDN -->
<link rel="stylesheet" href="//demo.productionready.io/main.css">
<link rel="stylesheet" href="/style.css">
`;
  },
  preloadTagForFile(file) {
    // add preload directives for JS assets and font assets, etc., skip for CSS files
    // this function will be called with each file that is procesed by Vite, including any files in your headTagsTemplate in your config
    return !file.endsWith(".css");
  },
};
