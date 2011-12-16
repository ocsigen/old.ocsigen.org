// This program was compiled from OCaml by js_of_ocaml 1.0
function caml_raise_with_arg (tag, arg) { throw [0, tag, arg]; }
function caml_raise_with_string (tag, msg) {
  caml_raise_with_arg (tag, new MlWrappedString (msg));
}
function caml_invalid_argument (msg) {
  caml_raise_with_string(caml_global_data[3], msg);
}
function caml_array_bound_error () {
  caml_invalid_argument("index out of bounds");
}
function caml_str_repeat(n, s) {
  if (!n) { return ""; }
  if (n & 1) { return caml_str_repeat(n - 1, s) + s; }
  var r = caml_str_repeat(n >> 1, s);
  return r + r;
}
function MlString(param) {
  if (param != null) {
    this.bytes = this.fullBytes = param;
    this.last = this.len = param.length;
  }
}
MlString.prototype = {
  string:null,
  bytes:null,
  fullBytes:null,
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
    this.bytes = this.fullBytes = b;
    this.last = this.len = b.length;
    return b;
  },
  getBytes:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return b;
  },
  getFullBytes:function() {
    var b = this.fullBytes;
    if (b !== null) return b;
    b = this.bytes;
    if (b == null) b = this.toBytes ();
    if (this.last < this.len) {
      this.bytes = (b += caml_str_repeat(this.len - this.last, '\0'));
      this.last = this.len;
    }
    this.fullBytes = b;
    return b;
  },
  toArray:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes ();
    var a = [], l = this.last;
    for (var i = 0; i < l; i++) a[i] = b.charCodeAt(i);
    for (l = this.len; i < l; i++) a[i] = 0;
    this.string = this.bytes = this.fullBytes = null;
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
    if (len !== null) return len;
    this.toBytes();
    return this.len;
  },
  toString:function() { var s = this.string; return s?s:this.toJsString(); },
  valueOf:function() { var s = this.string; return s?s:this.toJsString(); },
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
      this.bytes = this.fullBytes = this.string = null;
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
      this.bytes = this.fullBytes = this.string = null;
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
function caml_blit_string(s1, i1, s2, i2, len) {
  if (len === 0) return;
  if (i2 === s2.last && i1 === 0 && s1.last == len) {
    var s = s1.bytes;
    if (s !== null)
      s2.bytes += s1.bytes;
    else
      s2.bytes += s1.getBytes();
    s2.last += len;
    return;
  }
  var a = s2.array;
  if (!a) a = s2.toArray(); else { s2.bytes = s2.string = null; }
  s1.blitToArray (i1, a, i2, len);
}
function caml_call_gen(f, args) {
  if(f.fun)
    return caml_call_gen(f.fun, args);
  var n = f.length;
  var d = n - args.length;
  if (d == 0)
    return f.apply(null, args);
  else if (d < 0)
    return caml_call_gen(f.apply(null, args.slice(0,n)), args.slice(n));
  else
    return function (x){ return caml_call_gen(f, args.concat([x])); };
}
function caml_create_string(len) { return new MlMakeString(len); }
function caml_js_from_array(a) { return a.slice(1); }
function caml_js_pure_expr (f) { return f(); }
function caml_js_var(x) { return eval(x.toString()); }
function caml_js_wrap_callback(f) {
  var toArray = Array.prototype.slice;
  return function () {
    var args = (arguments.length > 0)?toArray.call (arguments):[0];
    return caml_call_gen(f, args);
  }
}
function caml_make_vect (len, init) {
  var b = [0]; for (var i = 1; i <= len; i++) b[i] = init; return b;
}
function caml_ml_out_channels_list () { return 0; }
var caml_global_data = [];
function caml_register_global (n, v) { caml_global_data[n] = v; }
var caml_named_values = {};
function caml_register_named_value(nm,v) {
  caml_named_values[nm] = v; return 0;
}
(function()
  {function aE(bY,bZ){return bY.length==1?bY(bZ):caml_call_gen(bY,[bZ]);}
   var a=[0,new MlString("Invalid_argument")],
    b=[0,new MlString("Assert_failure")];
   caml_register_global(5,[0,new MlString("Division_by_zero")]);
   caml_register_global(3,a);
   caml_register_global(2,[0,new MlString("Failure")]);
   var az=new MlString("Pervasives.do_at_exit"),
    ay=new MlString("String.sub"),ax=new MlString("div"),
    aw=new MlString("style"),av=new MlString("link"),
    au=new MlString("[oclosure]goog.events[/oclosure]"),
    at=new MlString("[oclosure]goog.editor.Field[/oclosure]"),
    as=new MlString("delayedchange"),
    ar=
     new MlString
      ("[oclosure]goog.editor.plugins.BasicTextFormatter[/oclosure]"),
    aq=new MlString("[oclosure]goog.editor.plugins.EnterHandler[/oclosure]"),
    ap=
     new MlString("[oclosure]goog.editor.plugins.HeaderFormatter[/oclosure]"),
    ao=new MlString("[oclosure]goog.editor.plugins.LoremIpsum[/oclosure]"),
    an=
     new MlString
      ("[oclosure]goog.editor.plugins.RemoveFormatting[/oclosure]"),
    am=new MlString("[oclosure]goog.editor.plugins.UndoRedo[/oclosure]"),
    al=new MlString("[oclosure]goog.editor.plugins.LinkBubble[/oclosure]"),
    ak=
     new MlString
      ("[oclosure]goog.editor.plugins.LinkDialogPlugin[/oclosure]"),
    aj=
     new MlString("[oclosure]goog.editor.plugins.ListTabHandler[/oclosure]"),
    ai=
     new MlString
      ("[oclosure]goog.editor.plugins.SpacesTabHandler[/oclosure]"),
    ah=new MlString("+undo"),ag=new MlString("+redo"),
    af=new MlString("+link"),ae=new MlString("+indent"),
    ad=new MlString("+outdent"),ac=new MlString("+removeFormat"),
    ab=new MlString("+strikeThrough"),aa=new MlString("+subscript"),
    $=new MlString("+superscript"),_=new MlString("+underline"),
    Z=new MlString("+bold"),Y=new MlString("+italic"),
    X=new MlString("+fontSize"),W=new MlString("+fontName"),
    V=new MlString("+foreColor"),U=new MlString("+backColor"),
    T=new MlString("+insertOrderedList"),
    S=new MlString("+insertUnorderedList"),R=new MlString("+justifyCenter"),
    Q=new MlString("+justifyRight"),P=new MlString("+justifyLeft"),
    O=new MlString("[oclosure]goog.ui.editor.ToolbarController[/oclosure]"),
    N=new MlString("[oclosure]goog.ui.editor.DefaultToolbar[/oclosure]"),
    M=new MlString("fieldContents"),L=[0,new MlString("editor.ml"),17,14],
    K=[0,new MlString("editor.ml"),14,52],
    J=[0,new MlString("editor.ml"),10,50],I=new MlString("stylesheet"),
    H=new MlString("rel"),G=new MlString("text/css"),F=new MlString("type"),
    E=new MlString("href"),D=new MlString("screen"),C=new MlString("media"),
    B=new MlString("head"),A=new MlString("../goog/css/button.css"),
    z=new MlString("../goog/css/dialog.css"),
    y=new MlString("../goog/css/linkbutton.css"),
    x=new MlString("../goog/css/menu.css"),
    w=new MlString("../goog/css/menuitem.css"),
    v=new MlString("../goog/css/menuseparator.css"),
    u=new MlString("../goog/css/tab.css"),
    t=new MlString("../goog/css/tabbar.css"),
    s=new MlString("../goog/css/toolbar.css"),
    r=new MlString("../goog/css/colormenubutton.css"),
    q=new MlString("../goog/css/palette.css"),
    p=new MlString("../goog/css/colorpalette.css"),
    o=new MlString("../goog/css/editor/bubble.css"),
    n=new MlString("../goog/css/editor/dialog.css"),
    m=new MlString("../goog/css/editor/linkdialog.css"),
    l=new MlString("../goog/css/editortoolbar.css"),
    k=
     new MlString
      ("#editMe { \n   width: 600px; \n   height: 300px; \n   background-color: white; \n   border: 1px solid grey; "),
    j=new MlString("editMe"),i=new MlString("Click here to edit"),
    h=new MlString("toolbar");
   function g(f)
    {var c=caml_ml_out_channels_list(0);
     for(;;){if(c){var d=c[2];try {}catch(e){}var c=d;continue;}return 0;}}
   caml_register_named_value(az,g);var aA=[0,0],aB=null,aG=undefined;
   function aF(aC,aD){return aC==aB?aE(aD,0):aC;}var aI=Array;
   aA[1]=
   [0,
    function(aH)
     {return aH instanceof aI?0:[0,new MlWrappedString(aH.toString())];},
    aA[1]];
   function aL(aK,aJ){return aK.createElement(aJ.toString());}
   var aM=window,aN=aM.document;function aP(aO){return aO;}
   function aV(aQ)
    {return caml_js_pure_expr
             (function(aU)
               {var aR=aQ.getLen()-21|0,aS=10;
                if(0<=aS&&0<=aR&&aS<=(aQ.getLen()-aR|0))
                 {var aT=caml_create_string(aR);
                  caml_blit_string(aQ,aS,aT,0,aR);return caml_js_var(aT);}
                throw [0,a,ay];});}
   var aW=aV(au),a0=aV(at);function aZ(aX,aY){return aX.registerPlugin(aY);}
   var a1=as.toString(),a2=aV(ar),a3=aV(aq),a4=aV(ap),a5=aV(ao),a6=aV(an),
    a7=aV(am),a8=aV(al),a9=aV(ak),a_=aV(aj),bu=aV(ai),bt=ah.toString(),
    bs=ag.toString(),br=af.toString(),bq=ae.toString(),bp=ad.toString(),
    bo=ac.toString(),bn=ab.toString(),bm=aa.toString(),bl=$.toString(),
    bk=_.toString(),bj=Z.toString(),bi=Y.toString(),bh=X.toString(),
    bg=W.toString(),bf=V.toString(),be=U.toString(),bd=T.toString(),
    bc=S.toString(),bb=R.toString(),ba=Q.toString(),a$=P.toString(),bv=aV(O),
    bA=aV(N);
   function bz(bx)
    {var bw=aL(aN,av);bw.setAttribute(H.toString(),I.toString());
     bw.setAttribute(F.toString(),G.toString());
     bw.setAttribute(E.toString(),bx.toString());
     bw.setAttribute(C.toString(),D.toString());
     var by=aN.getElementsByTagName(B.toString()).item(0);
     if(by===aG)throw [0,b,J];by.appendChild(bw);return 0;}
   function bE(bB)
    {function bD(bC){aM.alert(bB.toString());throw [0,b,K];}
     return aF(aN.getElementById(bB.toString()),bD);}
   bz(A);bz(z);bz(y);bz(x);bz(w);bz(v);bz(u);bz(t);bz(s);bz(r);bz(q);
   bz(p);bz(o);bz(n);bz(m);bz(l);var bF=aL(aN,aw);
   bF.appendChild(aN.createTextNode(k.toString()));
   var bG=new a0(j.toString(),aB);
   function bO(bN)
    {function bJ(bH){throw [0,b,L];}
     var bI=bE(M),bK=ax.toString(),bL=bI.tagName.toLowerCase()===bK?bI:aB,
      bM=aF(bL,bJ);
     return bM.innerHTML=bG.getCleanContents();}
   aZ(bG,new a2);aZ(bG,new a6);aZ(bG,new a7(aB));aZ(bG,new a_);aZ(bG,new bu);
   aZ(bG,new a3);aZ(bG,new a4);aZ(bG,new a5(i.toString()));aZ(bG,new a9);
   aZ(bG,new a8(caml_js_from_array([0])));
   var bP=[0,bj,bi,bk,bf,be,bg,bh,br,bt,bs,bc,bd,bq,bp,a$,bb,ba,bm,bl,bn,bo],
    bQ=bP.length-1;
   if(0===bQ)var bR=[0];else
    {var bS=caml_make_vect(bQ,aP(bP[0+1])),bT=1,bU=bQ-1|0;
     if(bT<=bU)
      {var bV=bT;
       for(;;)
        {bS[bV+1]=aP(bP[bV+1]);var bW=bV+1|0;if(bU!==bV){var bV=bW;continue;}
         break;}}
     var bR=bS;}
   var bX=caml_js_from_array(bR);new bv(bG,bA.makeToolbar(bX,bE(h),aB));
   aW.listen(aP(bG),a1,caml_js_wrap_callback(bO),aB);bG.makeEditable(aB);
   g(0);return;}
  ());
