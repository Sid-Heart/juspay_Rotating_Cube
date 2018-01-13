// Generated by purs bundle 0.11.7
var PS = {};
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var Semigroupoid = function (compose) {
      this.compose = compose;
  };
  var semigroupoidFn = new Semigroupoid(function (f) {
      return function (g) {
          return function (x) {
              return f(g(x));
          };
      };
  });
  var compose = function (dict) {
      return dict.compose;
  };
  exports["compose"] = compose;
  exports["Semigroupoid"] = Semigroupoid;
  exports["semigroupoidFn"] = semigroupoidFn;
})(PS["Control.Semigroupoid"] = PS["Control.Semigroupoid"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var Control_Semigroupoid = PS["Control.Semigroupoid"];        
  var Category = function (Semigroupoid0, id) {
      this.Semigroupoid0 = Semigroupoid0;
      this.id = id;
  };
  var id = function (dict) {
      return dict.id;
  };
  var categoryFn = new Category(function () {
      return Control_Semigroupoid.semigroupoidFn;
  }, function (x) {
      return x;
  });
  exports["Category"] = Category;
  exports["id"] = id;
  exports["categoryFn"] = categoryFn;
})(PS["Control.Category"] = PS["Control.Category"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var Control_Category = PS["Control.Category"];
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  exports["const"] = $$const;
})(PS["Data.Function"] = PS["Data.Function"] || {});
(function(exports) {
    "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
    "use strict";

  exports.showNumberImpl = function (n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Data.Show"];     
  var Show = function (show) {
      this.show = show;
  };                                                 
  var showNumber = new Show($foreign.showNumberImpl);
  var show = function (dict) {
      return dict.show;
  };
  exports["Show"] = Show;
  exports["show"] = show;
  exports["showNumber"] = showNumber;
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Data.Unit"];
  var Data_Show = PS["Data.Show"];
  exports["unit"] = $foreign.unit;
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Data.Functor"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Function = PS["Data.Function"];
  var Data_Unit = PS["Data.Unit"];        
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };
  var $$void = function (dictFunctor) {
      return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
  };
  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["void"] = $$void;
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];        
  var Apply = function (Functor0, apply) {
      this.Functor0 = Functor0;
      this.apply = apply;
  };                      
  var apply = function (dict) {
      return dict.apply;
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
})(PS["Control.Apply"] = PS["Control.Apply"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var Control_Apply = PS["Control.Apply"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var Applicative = function (Apply0, pure) {
      this.Apply0 = Apply0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["pure"] = pure;
  exports["liftA1"] = liftA1;
})(PS["Control.Applicative"] = PS["Control.Applicative"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Control.Bind"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var Bind = function (Apply0, bind) {
      this.Apply0 = Apply0;
      this.bind = bind;
  };                     
  var bind = function (dict) {
      return dict.bind;
  };
  exports["Bind"] = Bind;
  exports["bind"] = bind;
})(PS["Control.Bind"] = PS["Control.Bind"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var Monad = function (Applicative0, Bind1) {
      this.Applicative0 = Applicative0;
      this.Bind1 = Bind1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad.Bind1())(f)(function (v) {
                  return Control_Bind.bind(dictMonad.Bind1())(a)(function (v1) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(v(v1));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS["Control.Monad"] = PS["Control.Monad"] || {});
(function(exports) {
    "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };

  exports.forE = function (lo) {
    return function (hi) {
      return function (f) {
        return function () {
          for (var i = lo; i < hi; i++) {
            f(i)();
          }
        };
      };
    };
  };
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Control.Monad.Eff"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad = PS["Control.Monad"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var monadEff = new Control_Monad.Monad(function () {
      return applicativeEff;
  }, function () {
      return bindEff;
  });
  var bindEff = new Control_Bind.Bind(function () {
      return applyEff;
  }, $foreign.bindE);
  var applyEff = new Control_Apply.Apply(function () {
      return functorEff;
  }, Control_Monad.ap(monadEff));
  var applicativeEff = new Control_Applicative.Applicative(function () {
      return applyEff;
  }, $foreign.pureE);
  var functorEff = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEff));
  exports["functorEff"] = functorEff;
  exports["applyEff"] = applyEff;
  exports["applicativeEff"] = applicativeEff;
  exports["bindEff"] = bindEff;
  exports["monadEff"] = monadEff;
  exports["forE"] = $foreign.forE;
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
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
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Console"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  var logShow = function (dictShow) {
      return function (a) {
          return $foreign.log(Data_Show.show(dictShow)(a));
      };
  };
  exports["logShow"] = logShow;
  exports["log"] = $foreign.log;
})(PS["Control.Monad.Eff.Console"] = PS["Control.Monad.Eff.Console"] || {});
(function(exports) {
    "use strict";

  exports.newSTRef = function (val) {
    return function () {
      return { value: val };
    };
  };

  exports.readSTRef = function (ref) {
    return function () {
      return ref.value;
    };
  };

  exports.writeSTRef = function (ref) {
    return function (a) {
      return function () {
        return ref.value = a; // eslint-disable-line no-return-assign
      };
    };
  };
})(PS["Control.Monad.ST"] = PS["Control.Monad.ST"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Control.Monad.ST"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  exports["newSTRef"] = $foreign.newSTRef;
  exports["readSTRef"] = $foreign.readSTRef;
  exports["writeSTRef"] = $foreign.writeSTRef;
})(PS["Control.Monad.ST"] = PS["Control.Monad.ST"] || {});
(function(exports) {
  /* global window */
  "use strict";

  exports.window = function () {
    return window;
  };
})(PS["DOM.HTML"] = PS["DOM.HTML"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["DOM.HTML"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var DOM = PS["DOM"];
  var DOM_HTML_Types = PS["DOM.HTML.Types"];
  exports["window"] = $foreign.window;
})(PS["DOM.HTML"] = PS["DOM.HTML"] || {});
(function(exports) {
    "use strict";

  // module DOM.RequestAnimationFrame 

  var requestAnimationFrame = null;

  // http://www.paulirish.com/2011/requestanimationframe-for-smart-animating/
  exports.requestAnimationFrame_ = function(window_) {
      return function(action) {

          if (!requestAnimationFrame) {
              requestAnimationFrame = (function() {
                  return window_.requestAnimationFrame ||
                      window_.webkitRequestAnimationFrame ||
                      window_.mozRequestAnimationFrame ||
                      function(callback) {
                          window_.setTimeout(callback, 1000 / 60);
                      };
              })();
          }

          return function() {
              return requestAnimationFrame(action);
          };
      }
  };
})(PS["DOM.RequestAnimationFrame"] = PS["DOM.RequestAnimationFrame"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["DOM.RequestAnimationFrame"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var DOM = PS["DOM"];
  var DOM_HTML = PS["DOM.HTML"];
  var DOM_HTML_Types = PS["DOM.HTML.Types"];
  var Prelude = PS["Prelude"];        
  var requestAnimationFrame = function (action) {
      return function __do() {
          var v = DOM_HTML.window();
          return $foreign.requestAnimationFrame_(v)(action)();
      };
  };
  exports["requestAnimationFrame"] = requestAnimationFrame;
})(PS["DOM.RequestAnimationFrame"] = PS["DOM.RequestAnimationFrame"] || {});
(function(exports) {
    "use strict";

  //------------------------------------------------------------------------------
  // Array size ------------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.length = function (xs) {
    return xs.length;
  };

  //------------------------------------------------------------------------------
  // Indexed operations ----------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.indexImpl = function (just) {
    return function (nothing) {
      return function (xs) {
        return function (i) {
          return i < 0 || i >= xs.length ? nothing :  just(xs[i]);
        };
      };
    };
  };
})(PS["Data.Array"] = PS["Data.Array"] || {});
(function(exports) {
    "use strict";

  exports.emptySTArray = function () {
    return [];
  };

  exports.peekSTArrayImpl = function (just) {
    return function (nothing) {
      return function (xs) {
        return function (i) {
          return function () {
            return i >= 0 && i < xs.length ? just(xs[i]) : nothing;
          };
        };
      };
    };
  };

  exports.pokeSTArray = function (xs) {
    return function (i) {
      return function (a) {
        return function () {
          var ret = i >= 0 && i < xs.length;
          if (ret) xs[i] = a;
          return ret;
        };
      };
    };
  };

  exports.pushAllSTArray = function (xs) {
    return function (as) {
      return function () {
        return xs.push.apply(xs, as);
      };
    };
  };
})(PS["Data.Array.ST"] = PS["Data.Array.ST"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Category = PS["Control.Category"];
  var Control_Extend = PS["Control.Extend"];
  var Control_Monad = PS["Control.Monad"];
  var Control_MonadZero = PS["Control.MonadZero"];
  var Control_Plus = PS["Control.Plus"];
  var Data_Bounded = PS["Data.Bounded"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Functor_Invariant = PS["Data.Functor.Invariant"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  var Prelude = PS["Prelude"];        
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  var maybe = function (v) {
      return function (v1) {
          return function (v2) {
              if (v2 instanceof Nothing) {
                  return v;
              };
              if (v2 instanceof Just) {
                  return v1(v2.value0);
              };
              throw new Error("Failed pattern match at Data.Maybe line 219, column 1 - line 219, column 51: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  };
  var fromMaybe = function (a) {
      return maybe(a)(Control_Category.id(Control_Category.categoryFn));
  };
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
  exports["maybe"] = maybe;
  exports["fromMaybe"] = fromMaybe;
})(PS["Data.Maybe"] = PS["Data.Maybe"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Data.Array.ST"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_ST = PS["Control.Monad.ST"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Maybe = PS["Data.Maybe"];
  var Prelude = PS["Prelude"];
  var Unsafe_Coerce = PS["Unsafe.Coerce"];
  var pushSTArray = function (arr) {
      return function (a) {
          return $foreign.pushAllSTArray(arr)([ a ]);
      };
  };
  var peekSTArray = $foreign.peekSTArrayImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  var modifySTArray = function (xs) {
      return function (i) {
          return function (f) {
              return function __do() {
                  var v = peekSTArray(xs)(i)();
                  if (v instanceof Data_Maybe.Just) {
                      return $foreign.pokeSTArray(xs)(i)(f(v.value0))();
                  };
                  if (v instanceof Data_Maybe.Nothing) {
                      return false;
                  };
                  throw new Error("Failed pattern match at Data.Array.ST line 120, column 3 - line 122, column 26: " + [ v.constructor.name ]);
              };
          };
      };
  };
  exports["peekSTArray"] = peekSTArray;
  exports["pushSTArray"] = pushSTArray;
  exports["modifySTArray"] = modifySTArray;
  exports["emptySTArray"] = $foreign.emptySTArray;
})(PS["Data.Array.ST"] = PS["Data.Array.ST"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Data.Array"];
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Category = PS["Control.Category"];
  var Control_Lazy = PS["Control.Lazy"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Rec_Class = PS["Control.Monad.Rec.Class"];
  var Control_Monad_ST = PS["Control.Monad.ST"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Array_ST = PS["Data.Array.ST"];
  var Data_Array_ST_Iterator = PS["Data.Array.ST.Iterator"];
  var Data_Boolean = PS["Data.Boolean"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_HeytingAlgebra = PS["Data.HeytingAlgebra"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_NonEmpty = PS["Data.NonEmpty"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Ring = PS["Data.Ring"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Semiring = PS["Data.Semiring"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Unfoldable = PS["Data.Unfoldable"];
  var Partial_Unsafe = PS["Partial.Unsafe"];
  var Prelude = PS["Prelude"];
  var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  exports["index"] = index;
})(PS["Data.Array"] = PS["Data.Array"] || {});
(function(exports) {
    "use strict";

  exports.on = function(evt) {
      return function(act) {
          return function(ob) {
              return function() {
                  ob.on(evt, function(e) {
                      act(e)(jQuery(this))();
                  });
              };
          };
      };
  };


  exports.getPageX = function(e) {
      return function() {
          return e.pageX;
      };
  };

  exports.getElementById = function(name) {
      return function() {
          return jQuery("#" + name);
      };
  };

  exports.getPageY = function(e) {
      return function() {
          return e.pageY;
      };
  };
})(PS["Element"] = PS["Element"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Element"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var DOM = PS["DOM"];
  var Data_Foreign = PS["Data.Foreign"];
  var Prelude = PS["Prelude"];
  exports["on"] = $foreign.on;
  exports["getPageX"] = $foreign.getPageX;
  exports["getElementById"] = $foreign.getElementById;
  exports["getPageY"] = $foreign.getPageY;
})(PS["Element"] = PS["Element"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports.getCanvasElementByIdImpl = function(id, Just, Nothing) {
      return function() {
          var el = document.getElementById(id);
          if (el && el instanceof HTMLCanvasElement) {
              return Just(el);
          } else {
              return Nothing;
          }
      };
  };

  exports.getContext2D = function(c) {
      return function() {
          return c.getContext('2d');
      };
  };

  exports.getCanvasWidth = function(canvas) {
      return function() {
          return canvas.width;
      };
  };

  exports.getCanvasHeight = function(canvas) {
      return function() {
          return canvas.height;
      };
  };

  exports.setStrokeStyle = function(style) {
      return function(ctx) {
          return function() {
              ctx.strokeStyle = style;
              return ctx;
          };
      };
  };

  exports.beginPath = function(ctx) {
      return function() {
          ctx.beginPath();
          return ctx;
      };
  };

  exports.stroke = function(ctx) {
      return function() {
          ctx.stroke();
          return ctx;
      };
  };

  exports.lineTo = function(ctx) {
      return function(x) {
          return function(y) {
              return function() {
                  ctx.lineTo(x, y);
                  return ctx;
              };
          };
      };
  };

  exports.moveTo = function(ctx) {
      return function(x) {
          return function(y) {
              return function() {
                  ctx.moveTo(x, y);
                  return ctx;
              };
          };
      };
  };

  exports.closePath = function(ctx) {
      return function() {
          ctx.closePath();
          return ctx;
      };
  };

  exports.translate = function(t) {
      return function(ctx) {
          return function() {
              ctx.translate(t.translateX, t.translateY);
              return ctx;
          };
      };
  };

  exports.clearRect = function(ctx) {
      return function(r) {
          return function() {
              ctx.clearRect(r.x, r.y, r.w, r.h);
              return ctx;
          };
      };
  };
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Graphics.Canvas"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Exception_Unsafe = PS["Control.Monad.Eff.Exception.Unsafe"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_ArrayBuffer_Types = PS["Data.ArrayBuffer.Types"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Function_Uncurried = PS["Data.Function.Uncurried"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Prelude = PS["Prelude"];
  var strokePath = function (ctx) {
      return function (path) {
          return function __do() {
              var v = $foreign.beginPath(ctx)();
              var v1 = path();
              var v2 = $foreign.stroke(ctx)();
              return v1;
          };
      };
  };
  var getCanvasElementById = function (elId) {
      return $foreign.getCanvasElementByIdImpl(elId, Data_Maybe.Just.create, Data_Maybe.Nothing.value);
  };
  exports["getCanvasElementById"] = getCanvasElementById;
  exports["strokePath"] = strokePath;
  exports["getContext2D"] = $foreign.getContext2D;
  exports["getCanvasWidth"] = $foreign.getCanvasWidth;
  exports["getCanvasHeight"] = $foreign.getCanvasHeight;
  exports["setStrokeStyle"] = $foreign.setStrokeStyle;
  exports["lineTo"] = $foreign.lineTo;
  exports["moveTo"] = $foreign.moveTo;
  exports["closePath"] = $foreign.closePath;
  exports["clearRect"] = $foreign.clearRect;
  exports["translate"] = $foreign.translate;
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
    "use strict";          

  exports.cos = Math.cos;    

  exports.sin = Math.sin;      

  exports.pi = Math.PI;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var $foreign = PS["Math"];
  exports["cos"] = $foreign.cos;
  exports["sin"] = $foreign.sin;
  exports["pi"] = $foreign.pi;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
  // Generated by purs version 0.11.7
  "use strict";
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Console = PS["Control.Monad.Eff.Console"];
  var Control_Monad_Eff_JQuery = PS["Control.Monad.Eff.JQuery"];
  var Control_Monad_Eff_Ref = PS["Control.Monad.Eff.Ref"];
  var Control_Monad_ST = PS["Control.Monad.ST"];
  var DOM = PS["DOM"];
  var DOM_RequestAnimationFrame = PS["DOM.RequestAnimationFrame"];
  var Data_Array = PS["Data.Array"];
  var Data_Array_ST = PS["Data.Array.ST"];
  var Data_EuclideanRing = PS["Data.EuclideanRing"];
  var Data_Foreign = PS["Data.Foreign"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Int = PS["Data.Int"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Ring = PS["Data.Ring"];
  var Data_Semiring = PS["Data.Semiring"];
  var Data_Show = PS["Data.Show"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Unit = PS["Data.Unit"];
  var Element = PS["Element"];
  var Global_Unsafe = PS["Global.Unsafe"];
  var Graphics_Canvas = PS["Graphics.Canvas"];
  var Graphics_Drawing = PS["Graphics.Drawing"];
  var $$Math = PS["Math"];
  var OutWatch_Helpers_Helpers = PS["OutWatch.Helpers.Helpers"];
  var Partial_Unsafe = PS["Partial.Unsafe"];
  var Prelude = PS["Prelude"];        
  var rotateY = function (angle) {
      return function (x) {
          return function (y) {
              return function (z) {
                  var rad = (angle * $$Math.pi) / 180.0;
                  var xx = z * $$Math.sin(rad) + x * $$Math.cos(rad);
                  var zz = z * $$Math.cos(rad) - x * $$Math.sin(rad);
                  return [ xx, y, zz ];
              };
          };
      };
  };
  var rotateX = function (angle) {
      return function (x) {
          return function (y) {
              return function (z) {
                  var rad = (angle * $$Math.pi) / 180.0;
                  var yy = y * $$Math.cos(rad) - z * $$Math.sin(rad);
                  var zz = y * $$Math.sin(rad) + z * $$Math.cos(rad);
                  return [ x, yy, zz ];
              };
          };
      };
  };
  var mouseUp = function (input) {
      return function (rxd) {
          return function (ryd) {
              return function (rx) {
                  return function (ry) {
                      return function (e) {
                          return function (v) {
                              return function __do() {
                                  var v1 = Element.getPageX(e)();
                                  var v2 = Element.getPageY(e)();
                                  Control_Monad_Eff_Console.logShow(Data_Show.showNumber)(v1)();
                                  Control_Monad_Eff_Console.logShow(Data_Show.showNumber)(v2)();
                                  var v3 = Control_Monad_ST.readSTRef(rxd)();
                                  var v4 = Control_Monad_ST.readSTRef(ryd)();
                                  var v5 = Control_Monad_ST.writeSTRef(ry)((v1 - v3) / 10.0)();
                                  var v6 = Control_Monad_ST.writeSTRef(rx)((600.0 - v2 - v4) / 10.0)();
                                  Control_Monad_Eff_Console.logShow(Data_Show.showNumber)((v1 - v3) / 10.0)();
                                  Control_Monad_Eff_Console.logShow(Data_Show.showNumber)((600.0 - v2 - v4) / 10.0)();
                                  return Data_Unit.unit;
                              };
                          };
                      };
                  };
              };
          };
      };
  };
  var mouseDown = function (input) {
      return function (oldx) {
          return function (oldy) {
              return function (e) {
                  return function (v) {
                      return function __do() {
                          Control_Monad_Eff_Console.log("MouseDown")();
                          var v1 = Element.getPageX(e)();
                          var v2 = Element.getPageY(e)();
                          Control_Monad_Eff_Console.logShow(Data_Show.showNumber)(v1)();
                          Control_Monad_Eff_Console.logShow(Data_Show.showNumber)(v2)();
                          var v3 = Control_Monad_ST.writeSTRef(oldx)(v1)();
                          var v4 = Control_Monad_ST.writeSTRef(oldy)(600.0 - v2)();
                          return Data_Unit.unit;
                      };
                  };
              };
          };
      };
  };
  var drawLine = function (sx) {
      return function (sy) {
          return function (ex) {
              return function (ey) {
                  return function (ctx) {
                      return Graphics_Canvas.strokePath(ctx)(function __do() {
                          var v = Graphics_Canvas.setStrokeStyle("#FF00FF")(ctx)();
                          var v1 = Graphics_Canvas.moveTo(ctx)(sx)(sy)();
                          var v2 = Graphics_Canvas.lineTo(ctx)(ex)(ey)();
                          var v3 = Graphics_Canvas.closePath(ctx)();
                          var v4 = Data_Array_ST.emptySTArray();
                          return Data_Functor["void"](Control_Monad_Eff.functorEff)(Data_Array_ST.pushSTArray(v4)(1))();
                      });
                  };
              };
          };
      };
  };
  var drawCube = function (st_nodes) {
      return function (st_edges) {
          return function (ctx) {
              return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_Eff.forE(0)(12)(function (i) {
                  return function __do() {
                      var v = Data_Array_ST.peekSTArray(st_edges)(i)();
                      var i_edge = Data_Maybe.fromMaybe([  ])(v);
                      var i_edge_s = Data_Array.index(i_edge)(0);
                      var i_edge_e = Data_Array.index(i_edge)(1);
                      var e_s = Data_Maybe.fromMaybe(0)(i_edge_s);
                      var e_e = Data_Maybe.fromMaybe(0)(i_edge_e);
                      var v1 = Data_Array_ST.peekSTArray(st_nodes)(e_s)();
                      var s_node = Data_Maybe.fromMaybe([  ])(v1);
                      var s_x_temp = Data_Array.index(s_node)(0);
                      var s_y_temp = Data_Array.index(s_node)(1);
                      var s_x = Data_Maybe.fromMaybe(0.0)(s_x_temp);
                      var s_y = Data_Maybe.fromMaybe(0.0)(s_y_temp);
                      var v2 = Data_Array_ST.peekSTArray(st_nodes)(e_e)();
                      var e_node = Data_Maybe.fromMaybe([  ])(v2);
                      var e_x_temp = Data_Array.index(e_node)(0);
                      var e_y_temp = Data_Array.index(e_node)(1);
                      var e_x = Data_Maybe.fromMaybe(0.0)(e_x_temp);
                      var e_y = Data_Maybe.fromMaybe(0.0)(e_y_temp);
                      return drawLine(s_x)(s_y)(e_x)(e_y)(ctx)();
                  };
              }));
          };
      };
  };
  var dampingfactor = function (x) {
      return x * 0.9;
  };
  var main = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
      var v = Control_Monad_ST.newSTRef(0.0)();
      var v1 = Control_Monad_ST.newSTRef(0.0)();
      var v2 = Control_Monad_ST.newSTRef(0.0)();
      var v3 = Control_Monad_ST.newSTRef(0.0)();
      var v4 = Graphics_Canvas.getCanvasElementById("canvas")();
      var __unused = function (dictPartial1) {
          return function ($dollar40) {
              return $dollar40;
          };
      };
      return __unused()((function () {
          if (v4 instanceof Data_Maybe.Just) {
              return function __do() {
                  var v5 = Graphics_Canvas.getContext2D(v4.value0)();
                  var v6 = Element.getElementById("canvas")();
                  var v7 = Graphics_Canvas.translate({
                      translateX: 250.0,
                      translateY: 250.0
                  })(v5)();
                  var msz = -100.0;
                  var nodes = [ [ msz, msz, msz ], [ msz, msz, 100.0 ], [ msz, 100.0, msz ], [ msz, 100.0, 100.0 ], [ 100.0, msz, msz ], [ 100.0, msz, 100.0 ], [ 100.0, 100.0, msz ], [ 100.0, 100.0, 100.0 ] ];
                  var edges = [ [ 0, 1 ], [ 1, 3 ], [ 3, 2 ], [ 2, 0 ], [ 4, 5 ], [ 5, 7 ], [ 7, 6 ], [ 6, 4 ], [ 0, 4 ], [ 1, 5 ], [ 2, 6 ], [ 3, 7 ] ];
                  var v8 = Data_Array_ST.emptySTArray();
                  Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_Eff.forE(0)(8)(function (i) {
                      return function __do() {
                          Control_Monad_Eff_Console.log("Resetting")();
                          var xx = Data_Array.index(nodes)(i);
                          var yy = Data_Maybe.fromMaybe([  ])(xx);
                          return Data_Functor["void"](Control_Monad_Eff.functorEff)(Data_Array_ST.pushSTArray(v8)(yy))();
                      };
                  }))();
                  var v9 = Data_Array_ST.emptySTArray();
                  Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_Eff.forE(0)(12)(function (i) {
                      var xx = Data_Array.index(edges)(i);
                      var yy = Data_Maybe.fromMaybe([  ])(xx);
                      return Data_Functor["void"](Control_Monad_Eff.functorEff)(Data_Array_ST.pushSTArray(v9)(yy));
                  }))();
                  var updateCube = function __do() {
                      var v10 = Data_Array_ST.emptySTArray();
                      Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_Eff.forE(0)(8)(function (i) {
                          return function __do() {
                              var v11 = Data_Array_ST.peekSTArray(v8)(i)();
                              var vi = Data_Maybe.fromMaybe([  ])(v11);
                              var mx = Data_Array.index(vi)(0);
                              var my = Data_Array.index(vi)(1);
                              var mz = Data_Array.index(vi)(2);
                              var x = Data_Maybe.fromMaybe(0.0)(mx);
                              var y = Data_Maybe.fromMaybe(0.0)(my);
                              var z = Data_Maybe.fromMaybe(0.0)(mz);
                              var v12 = Control_Monad_ST.readSTRef(v2)();
                              var v13 = Control_Monad_ST.readSTRef(v3)();
                              var v14 = rotateX(v12)(x)(y)(z);
                              var mvx = Data_Array.index(v14)(0);
                              var mvy = Data_Array.index(v14)(1);
                              var mvz = Data_Array.index(v14)(2);
                              var vx = Data_Maybe.fromMaybe(0.0)(mvx);
                              var vy = Data_Maybe.fromMaybe(0.0)(mvy);
                              var vz = Data_Maybe.fromMaybe(0.0)(mvz);
                              var vv = rotateY(v13)(vx)(vy)(vz);
                              return Data_Functor["void"](Control_Monad_Eff.functorEff)(Data_Array_ST.pushSTArray(v10)(vv))();
                          };
                      }))();
                      var v11 = Control_Monad_ST.readSTRef(v2)();
                      var v12 = Control_Monad_ST.readSTRef(v3)();
                      Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_ST.writeSTRef(v2)(dampingfactor(v11)))();
                      Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_ST.writeSTRef(v3)(dampingfactor(v12)))();
                      Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_Eff.forE(0)(8)(function (i) {
                          return function __do() {
                              var v13 = Data_Array_ST.peekSTArray(v10)(i)();
                              var vi = Data_Maybe.fromMaybe([  ])(v13);
                              return Data_Functor["void"](Control_Monad_Eff.functorEff)(Data_Array_ST.modifySTArray(v8)(i)(function (mf) {
                                  return vi;
                              }))();
                          };
                      }))();
                      var v13 = Graphics_Canvas.getCanvasWidth(v4.value0)();
                      var v14 = Graphics_Canvas.getCanvasHeight(v4.value0)();
                      Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.clearRect(v5)({
                          x: -300.0,
                          y: -300.0,
                          w: v13,
                          h: v14
                      }))();
                      drawCube(v10)(v9)(v5)();
                      return DOM_RequestAnimationFrame.requestAnimationFrame(updateCube)();
                  };
                  updateCube();
                  Element.on("mousedown")(mouseDown(v4.value0)(v)(v1))(v6)();
                  return Element.on("mouseup")(mouseUp(v4.value0)(v)(v1)(v2)(v3))(v6)();
              };
          };
          throw new Error("Failed pattern match at Main line 122, column 3 - line 123, column 3: " + [ v4.constructor.name ]);
      })())();
  });
  exports["drawLine"] = drawLine;
  exports["rotateX"] = rotateX;
  exports["rotateY"] = rotateY;
  exports["dampingfactor"] = dampingfactor;
  exports["drawCube"] = drawCube;
  exports["mouseDown"] = mouseDown;
  exports["mouseUp"] = mouseUp;
  exports["main"] = main;
})(PS["Main"] = PS["Main"] || {});
PS["Main"].main();