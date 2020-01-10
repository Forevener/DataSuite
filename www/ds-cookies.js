/* inspired by https://calligross.de/post/using-cookie-based-authentication-with-shiny */
/* based on https://learn.javascript.ru/cookie */
shinyjs.getCookie = function (params) {
	var defaultParams = {
        name : "cookie"
    };
    params = shinyjs.getParams(params, defaultParams);
	  
    var matches = document.cookie.match(new RegExp(
      "(?:^|; )" + params.name.replace(/([\.$?*|{}\(\)\[\]\\\/\+^])/g, '\\$1') + "=([^;]*)"
    ));
    Shiny.onInputChange("cookie_" + params.name, matches ? decodeURIComponent(matches[1]) : null);
}

shinyjs.setCookie = function (params) {
	var defaultParams = {
        name : "cookie",
        value : 0,
		expiration: -1
    };
    params = shinyjs.getParams(params, defaultParams);
	  
	options = {
      path: '/',
	  expires: params.expiration
    };

    if (options.expires.toUTCString) {
      options.expires = options.expires.toUTCString();
    }

    updatedCookie = encodeURIComponent(params.name) + "=" + encodeURIComponent(params.value);

    for (var optionKey in options) {
        updatedCookie += "; " + optionKey;
        optionValue = options[optionKey];
        if (optionValue !== true) {
            updatedCookie += "=" + optionValue;
        }
    }

    document.cookie = updatedCookie;
    // Shiny.onInputChange("cookie", true);
}

shinyjs.remCookie = function (params) {
	var defaultParams = {
        name : "cookie"
    };
    params = shinyjs.getParams(params, defaultParams);
	  
    options = {
      'path': '/',
	  'max-age': -1
    };

    updatedCookie = encodeURIComponent(params.name) + "=" + encodeURIComponent(0);

    for (var optionKey in options) {
        updatedCookie += "; " + optionKey;
        optionValue = options[optionKey];
        if (optionValue !== true) {
            updatedCookie += "=" + optionValue;
        }
    }

    document.cookie = updatedCookie;
	// Shiny.onInputChange("cookie", true);
}