// This program was compiled from OCaml by js_of_ocaml 0.1
function caml_str_repeat(n, s) {
  if (!n) { return ""; }
  if (n & 1) { return caml_str_repeat(n - 1, s) + s; }
  var r = caml_str_repeat(n >> 1, s);
  return r + r;
}
function MlString(param) {
  if (param != null) {
    this.bytes = param;
    this.last = this.len = param.length;
  }
}
MlString.prototype = {
  string:null,
  bytes:null,
  array:null,
  len:null,
  last:0,
  toJsString:function() {
    return this.string = decodeURIComponent (escape(this.getFullBytes()));
  },
  toBytes:function() {
    if (this.string != null)
      var b = unescape (encodeURIComponent (this.string));
    else {
      var b = "", a = this.array, l = a.length;
      for (var i = 0; i < l; i ++) b += String.fromCharCode (a[i]);
    }
    this.bytes = b;
    this.last = this.len = b.length;
    return b;
  },
  getBytes:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return b;
  },
  getFullBytes:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes ();
    if (this.last < this.len) {
      this.bytes = (b += caml_str_repeat(this.len - this.last, '\0'));
      this.last = this.len;
    }
    return b;
  },
  toArray:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes ();
    var a = [], l = this.last;
    for (var i = 0; i < l; i++) a[i] = b.charCodeAt(i);
    for (l = this.len; i < l; i++) a[i] = 0;
    this.string = this.bytes = null;
    this.last = this.len;
    this.array = a;
    return a;
  },
  getArray:function() {
    var a = this.array;
    if (!a) a = this.toArray();
    return a;
  },
  getLen:function() {
    var len = this.len;
    if (len != null) return len;
    this.toBytes();
    return this.len;
  },
  toString:function() { var s = this.string; return s?s:this.toJsString(); },
  valueOf:function() { var s = this.string; return s?s:this.toJsString(); },
  blit:function(i1, s2, i2, l) {
    if (l == 0) return 0;
    if (s2.bytes != null && i2 == s2.last && this.len == l && this.last == l) {
      s2.bytes += this.getBytes();
      s2.last += l;
      return 0;
    }
    var a = s2.array;
    if (!a) a = s2.toArray(); else { s2.bytes = s2.string = null; }
    this.blitToArray (i1, a, i2, l);
  },
  blitToArray:function(i1, a2, i2, l) {
    var a1 = this.array;
    if (a1)
      for (var i = 0; i < l; i++) a2 [i2 + i] = a1 [i1 + i];
    else {
      var b = this.bytes;
      if (b == null) b = this.toBytes();
      var l1 = this.last - i1;
      if (l <= l1)
        for (var i = 0; i < l; i++) a2 [i2 + i] = b.charCodeAt(i1 + i);
      else {
        for (var i = 0; i < l1; i++) a2 [i2 + i] = b.charCodeAt(i1 + i);
        for (; i < l; i++) a2 [i2 + i] = 0;
      }
    }
  },
  get:function (i) {
    var a = this.array;
    if (a) return a[i];
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return (i<this.last)?b.charCodeAt(i):0;
  },
  safeGet:function (i) {
    if (!this.len) this.toBytes();
    if ((i < 0) || (i >= this.len)) caml_array_bound_error ();
    return this.get(i);
  },
  set:function (i, c) {
    var a = this.array;
    if (!a) {
      if (this.last == i) {
        this.bytes += String.fromCharCode (c & 0xff);
        this.last ++;
        return 0;
      }
      a = this.toArray();
    } else if (this.bytes != null) {
      this.bytes = this.string = null;
    }
    a[i] = c & 0xff;
    return 0;
  },
  safeSet:function (i, c) {
    if (this.len == null) this.toBytes ();
    if ((i < 0) || (i >= this.len)) caml_array_bound_error ();
    this.set(i, c);
  },
  fill:function (ofs, len, c) {
    if (ofs >= this.last && this.last && c == 0) return;
    var a = this.array;
    if (!a) a = this.toArray();
    else if (this.bytes != null) {
      this.bytes = this.string = null;
    }
    var l = ofs + len;
    for (i = ofs; i < l; i++) a[i] = c;
  },
  compare:function (s2) {
    if (this.string != null && s2.string != null) {
      if (this.string < s2.string) return -1;
      if (this.string > s2.string) return 1;
      return 0;
    }
    var b1 = this.getFullBytes ();
    var b2 = s2.getFullBytes ();
    if (b1 < b2) return -1;
    if (b1 > b2) return 1;
    return 0;
  },
  equal:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string == s2.string;
    return this.getFullBytes () == s2.getFullBytes ();
  },
  lessThan:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string < s2.string;
    return this.getFullBytes () < s2.getFullBytes ();
  },
  lessEqual:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string <= s2.string;
    return this.getFullBytes () <= s2.getFullBytes ();
  }
}
function MlWrappedString (s) { this.string = s; }
MlWrappedString.prototype = new MlString();
function MlMakeString (l) { this.bytes = ""; this.len = l; }
MlMakeString.prototype = new MlString ();
function caml_blit_string(s1, i1, s2, i2, len) { s1.blit (i1, s2, i2, len); }
function caml_create_string(len) { return new MlMakeString(len); }
function caml_js_pure_expr (f) { return f(); }
function caml_js_var(x) { return eval(x.toString()); }
function caml_call_gen(f, args) {
  var n = f.length;
  var d = n - args.length;
  if (d == 0)
    return f.apply(null, args);
  else if (d < 0)
    return caml_call_gen(f.apply(null, args.slice(0,n)), args.slice(n));
  else
    return function (x){ return caml_call_gen(f, args.concat([x])); };
}
function caml_js_wrap_callback(f) {
  var toArray = Array.prototype.slice;
  return function () {
    var args = (arguments.length > 0)?toArray.call (arguments):[0];
    return caml_call_gen(f, args);
  }
}
function caml_ml_out_channels_list () { return 0; }
var caml_global_data = [];
function caml_register_global (n, v) { caml_global_data[n] = v; }
var caml_named_values = {};
function caml_register_named_value(nm,v) {
  caml_named_values[nm] = v; return 0;
}
(function()
  {var a=[0,new MlString("Invalid_argument")];
   caml_register_global(5,[0,new MlString("Division_by_zero")]);
   caml_register_global(3,a);
   caml_register_global(2,[0,new MlString("Failure")]);
   var T=[0,new MlString("Assert_failure")],
    S=new MlString("Pervasives.do_at_exit"),R=new MlString("String.sub"),
    Q=new MlString("link"),
    P=new MlString("[oclosure]goog.events[/oclosure]"),
    O=new MlString("[oclosure]goog.ui.MenuItem[/oclosure]"),
    N=new MlString("[oclosure]goog.ui.Menu[/oclosure]"),
    M=new MlString("[oclosure]goog.ui.Toolbar[/oclosure]"),
    L=new MlString("[oclosure]goog.ui.ToolbarButton[/oclosure]"),
    K=new MlString("[oclosure]goog.ui.ToolbarMenuButton[/oclosure]"),
    J=new MlString("action"),I=new MlString("action"),
    H=[0,new MlString("oclosure_menu.ml"),22,59],
    G=new MlString("stylesheet"),F=new MlString("rel"),
    E=new MlString("text/css"),D=new MlString("type"),C=new MlString("href"),
    B=new MlString("screen"),A=new MlString("media"),z=new MlString("head"),
    y=
     new MlString
      ("/oclosure/goog/css/common.css"),
    x=
     new MlString
      ("/oclosure/goog/css/menu.css"),
    w=
     new MlString
      ("/oclosure/goog/css/menuitem.css"),
    v=
     new MlString
      ("/oclosure/goog/css/toolbar.css"),
    u=new MlString("About"),t=new MlString("Examples"),
    s=new MlString("Install"),r=new MlString("Sources"),
    q=new MlString("/oclosure/dev/manual/"),p=new MlString("Overview"),
    o=new MlString("/oclosure/dev/api/"),n=new MlString("API Reference"),
    m=new MlString("Documentation"),l=new MlString("enter"),
    k=new MlString("oclosure_menu"),j=new MlString("/oclosure/"),
    i=new MlString("/oclosure/examples/"),
    h=new MlString("/oclosure/install"),
    g=new MlString("/oclosure/sources");
   function f(e)
    {var b=caml_ml_out_channels_list(0);
     for(;;){if(b){var c=b[2];try {}catch(d){}var b=c;continue;}return 0;}}
   caml_register_named_value(S,f);var U=null,V=true;function X(W){return W;}
   var Y=window,Z=Y.document;function $(_){return _;}
   function af(aa)
    {return caml_js_pure_expr
             (function(ae)
               {var ab=aa.getLen()-21|0,ac=10;
                if(0<=ac&&0<=ab&&((aa.getLen()-ab|0)<ac?0:1))
                 {var ad=caml_create_string(ab);
                  caml_blit_string(aa,ac,ad,0,ab);return caml_js_var(ad);}
                throw [0,a,R];});}
   var ag=af(P);function al(ak,aj,ai,ah){return ag.listen(ak,aj,ai,ah);}
   function ap(am,ao,an){return am.addChild(ao,an);}
   var aq=af(O),ar=af(N),at=af(M),as=af(L),ax=af(K);
   function aw(av)
    {var au=Z.createElement(Q.toString());
     au.setAttribute(F.toString(),G.toString());
     au.setAttribute(D.toString(),E.toString());
     au.setAttribute(C.toString(),av.toString());
     au.setAttribute(A.toString(),B.toString());
     return Z.getElementsByTagName(z.toString()).item(0).appendChild(au);}
   aw(y);aw(x);aw(w);aw(v);function az(ay){return ay.toString();}
   function aH(aA,aC,aG)
    {var aB=new aq(az(aA),U,U),
      aF=
       caml_js_wrap_callback
        (function(aD){return Y.location.href=aC.toString();}),
      aE=I.toString();
     al($(aB),aE,aF,U);return aG.addItem($(aB));}
   function aN(aK,aI)
    {var
      aM=
       caml_js_wrap_callback
        (function(aJ){return Y.location.href=aI.toString();}),
      aL=J.toString();
     al($(aK),aL,aM,U);return 0;}
   var aO=new at(U,U,U),aP=new as(az(u),U,U),aQ=new as(az(t),U,U),
    aR=new as(az(s),U,U),aS=new as(az(r),U,U),aT=new ar(U,U);
   aH(p,q,aT);aH(n,o,aT);
   var aU=X(aT),aV=new ax(az(m),aU,U,U),
    aY=caml_js_wrap_callback(function(aW){return aV.showMenu();}),
    aX=l.toString();
   al($(aV),aX,aY,U);var aZ=Z.getElementById(k.toString());
   if(aZ==U)throw [0,T,H];aN(aP,j);aN(aQ,i);aN(aR,h);aN(aS,g);ap(aO,aP,X(V));
   ap(aO,aQ,X(V));ap(aO,aV,X(V));ap(aO,aR,X(V));ap(aO,aS,X(V));
   aO.render(X(aZ));f(0);return;}
  ());
