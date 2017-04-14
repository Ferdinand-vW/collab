// Generated by psc-bundle 0.10.7
var PS = {};
(function(exports) {
    "use strict";

  // module Browser.Location

  exports.getLocation = function() {
      return document.location.href;
  }
})(PS["Browser.Location"] = PS["Browser.Location"] || {});
(function(exports) {
  // Generated by psc version 0.10.7
  "use strict";
  var $foreign = PS["Browser.Location"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  exports["getLocation"] = $foreign.getLocation;
})(PS["Browser.Location"] = PS["Browser.Location"] || {});
(function(exports) {
    "use strict";

  exports.log = function (s) {
    return function () {
      console.log(s);
      return {};
    };
  };
})(PS["Control.Monad.Eff.Console"] = PS["Control.Monad.Eff.Console"] || {});
(function(exports) {
    "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
  // Generated by psc version 0.10.7
  "use strict";
  var $foreign = PS["Data.Unit"];
  var Data_Show = PS["Data.Show"];
  exports["unit"] = $foreign.unit;
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
  // Generated by psc version 0.10.7
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Console"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  exports["log"] = $foreign.log;
})(PS["Control.Monad.Eff.Console"] = PS["Control.Monad.Eff.Console"] || {});
(function(exports) {
  // Generated by psc version 0.10.7
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Console = PS["Control.Monad.Eff.Console"];
  var Data_URI = PS["Data.URI"];
  var DOM_HTML_Location = PS["DOM.HTML.Location"];
  var Browser_Location = PS["Browser.Location"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Applicative = PS["Control.Applicative"];
  var Data_Unit = PS["Data.Unit"];        
  var main = function __do() {
      var v = Browser_Location.getLocation();
      Control_Monad_Eff_Console.log(v)();
      return Data_Unit.unit;
  };
  exports["main"] = main;
})(PS["Room.Main"] = PS["Room.Main"] || {});
(function(exports) {
  // Generated by psc version 0.10.7
  "use strict";
  var Prelude = PS["Prelude"];
  var Room_Main = PS["Room.Main"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Console = PS["Control.Monad.Eff.Console"];
  var Browser_Location = PS["Browser.Location"];        
  var main = Room_Main.main;
  exports["main"] = main;
})(PS["Main"] = PS["Main"] || {});
PS["Main"].main();