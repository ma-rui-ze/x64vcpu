namespace x64emu{typedef uint8_t e;typedef int8_t bf;typedef uint16_t Tc;typedef int16_t Zg;typedef uint32_t md;typedef int32_t id;typedef uint64_t i;typedef int64_t gb;enum an{Bm=0,Cb,ed,ii,Mg,Am,We,Ng,ym,zm};enum bn{Ve=0x00,wm=0x01,gi=0x02,ei=0x03,xm=0x04,Lg=0x05,hi=0x06,fi=0x07,km=0x04,mm=0x05,nm=0x06,lm=0x07,um=0x00,vm=0x01,om=0x02,pm=0x03,qm=0x04,rm=0x05,sm=0x06,tm=0x07,};enum Xm{wl=0,x=0,Q=0,h=0,mb,Cj,Rb,Ob,Nb,Pb,Wb,Sb,Tb,Ub,Qb,fc,Vb,Je,Ie,L,lg,vl,He,og,Sd,ze,nb,tb,Ce,gg,De,pg,qg,Fh,Ae,Be,td,rd,vd,sd,ud,Ee,jg,ql,Dh,K,ag,bg,Bh,ig,Wd,Vd,eg,mg,Td,wg,ng,Fe,Ge,kg,Rd,Ud,hg,rg,ye,dg,fg,cg,ue,tg,we,vg,ve,ug,Gh,Eh,xg,Ic,Jc,Gc,Hc,ub,Lc,Kc,db,eb,xe,Ke,se,te,_f,Zf,tc,Xf,Yf,Ch,sg,uc,Fj,hk,Gj,Hk,Ik,pk,Tj,ik,Wj,Xj,jk,al,bl,cl,dl,vk,wk,ak,bk,ck,dk,kk,lk,mk,nk,xk,ok,ll,Wk,Zk,Jj,Ej,el,kl,yk,Ck,Bk,Fk,Dk,Ek,Gk,Dj,nl,Nk,Kk,ml,Mk,Zj,qk,Lk,ol,Vk,Uk,Ok,Rk,Tk,Yj,ek,_j,Kj,Sk,Ak,Yk,zk,Xk,fk,gk,sk,tk,uk,Lj,Nj,Mj,Sj,Oj,Qj,Pj,Rj,fl,gl,hl,Uj,Vj,il,jl,rk,Jk,Pk,Qk,_k,Hj,Ij,Al,Cl,Ll,Kl,Ml,El,Dl,Jl,Il,Fl,Gl,Hl,Ul,Vl,Rl,Ql,Wl,Tl,Sl,Pl,Nl,Ol,Bl,Aj,Bj,tj,vj,uj,yl,xl,xj,wj,zl,tl,ul,pl,yj,rl,zj,sl,};enum Zm{Wh=0,ab,Se,f,o,be,Te,ae,l,D,wd,gc,Fb,s=0xA00,bb=0xA01,vb=0xA02,Yh=0xA03,ai=0xA04,Xh=0xA05,_h=0xA06,Zh=0xA07,_c=0xB00,Oc=0xB01,Pc=0xB02,Nc=0xB03,dd=0xB04,ad=0xB05,cd=0xB06,bd=0xB07,Eg=0xC04,Gg=0xC05,Hg=0xC06,Fg=0xC07,Ue=0xD04,em=0xD05,fm=0xD06,dm=0xD07,im=0xF00,gm=0xF01,jm=0xF02,hm=0xF03,Jg=0xF04,Kg=0xF05,Vh,Dg,Ig,bi,ci,di,cm,};enum Ym{Yl=0,b,ob,Pe,O,Zc,Qe,Bg,_d,C,Re,U,d,Cg,bm,_l,Sh,Nh,Mh,Uh,Kh,Qh,Jh,Lh,Ph,Oh,Th,Rh,Zl,am,};enum _m{Xl=0,Zd=0x01,Oe=0x02,Ag=0x04,Mc=0x08,Ne=0x10,Yc=0x20,Le=0x40,Xd=0x100,Yd=0x200,Ih=0x1000,Me=0x2000,zg=0x10000,yg=0x20000,Hh=0x8000000};enum Sm{re=0,oj,pj,qj};struct R{int cb;int Ci;struct{int J;int c;int X;}jf[4];R*vi;};extern const struct R sh[256];extern const struct R rh[256];extern const struct R Vm[256];const struct R sh[]={{Ob,1,{{f,b},{o,b}}},{Ob,1,{{f,d},{o,d}}},{Ob,1,{{o,b},{f,b}}},{Ob,1,{{o,d},{f,d}}},{Ob,0,{{s,b},{l,b}}},{Ob,0,{{s,d},{l,C}}},{x},{x},{Sb,1,{{f,b},{o,b}}},{Sb,1,{{f,d},{o,d}}},{Sb,1,{{o,b},{f,b}}},{Sb,1,{{o,d},{f,d}}},{Sb,0,{{s,b},{l,b}}},{Sb,0,{{s,d},{l,C}}},{x},{x},{Nb,1,{{f,b},{o,b}}},{Nb,1,{{f,d},{o,d}}},{Nb,1,{{o,b},{f,b}}},{Nb,1,{{o,d},{f,d}}},{Nb,0,{{s,b},{l,b}}},{Nb,0,{{s,d},{l,C}}},{x},{x},{Tb,1,{{f,b},{o,b}}},{Tb,1,{{f,d},{o,d}}},{Tb,1,{{o,b},{f,b}}},{Tb,1,{{o,d},{f,d}}},{Tb,0,{{s,b},{l,b}}},{Tb,0,{{s,d},{l,C}}},{x},{x},{Pb,1,{{f,b},{o,b}}},{Pb,1,{{f,d},{o,d}}},{Pb,1,{{o,b},{f,b}}},{Pb,1,{{o,d},{f,d}}},{Pb,0,{{s,b},{l,b}}},{Pb,0,{{s,d},{l,C}}},{x},{x},{Ub,1,{{f,b},{o,b}}},{Ub,1,{{f,d},{o,d}}},{Ub,1,{{o,b},{f,b}}},{Ub,1,{{o,d},{f,d}}},{Ub,0,{{s,b},{l,b}}},{Ub,0,{{s,d},{l,C}}},{x},{x},{Wb,1,{{f,b},{o,b}}},{Wb,1,{{f,d},{o,d}}},{Wb,1,{{o,b},{f,b}}},{Wb,1,{{o,d},{f,d}}},{Wb,0,{{s,b},{l,b}}},{Wb,0,{{s,d},{l,C}}},{x},{x},{Qb,1,{{f,b},{o,b}}},{Qb,1,{{f,d},{o,d}}},{Qb,1,{{o,b},{f,b}}},{Qb,1,{{o,d},{f,d}}},{Qb,0,{{s,b},{l,b}}},{Qb,0,{{s,d},{l,C}}},{x},{x},{Q},{Q},{Q},{Q},{Q},{Q},{Q},{Q},{Q},{Q},{Q},{Q},{Q},{Q},{Q},{Q},{nb,0,{{_c,U}}},{nb,0,{{Oc,U}}},{nb,0,{{Pc,U}}},{nb,0,{{Nc,U}}},{nb,0,{{dd,U}}},{nb,0,{{ad,U}}},{nb,0,{{cd,U}}},{nb,0,{{bd,U}}},{tb,0,{{_c,U}}},{tb,0,{{Oc,U}}},{tb,0,{{Pc,U}}},{tb,0,{{Nc,U}}},{tb,0,{{dd,U}}},{tb,0,{{ad,U}}},{tb,0,{{cd,U}}},{tb,0,{{bd,U}}},{x},{x},{x},{og,1,{{o,Bg},{f,Zc}}},{Q},{Q},{Q},{Q},{nb,0,{{l,Re}}},{De,1,{{o,d},{f,d},{l,C}}},{nb,0,{{l,Pe}}},{De,1,{{o,d},{f,d},{l,ob}}},{hg,0,{{Fb,b},{vb,O}}},{hg,0,{{Fb,_d},{vb,O}}},{rg,0,{{vb,O},{gc,b}}},{rg,0,{{vb,O},{gc,_d}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{K,0,{{D,b}}},{mb,1,{},(struct R[]){{Ob,1,{{f,b},{l,b}}},{Sb,1,{{f,b},{l,b}}},{Nb,1,{{f,b},{l,b}}},{Tb,1,{{f,b},{l,b}}},{Pb,1,{{f,b},{l,b}}},{Ub,1,{{f,b},{l,b}}},{Wb,1,{{f,b},{l,b}}},{Qb,1,{{f,b},{l,b}}},}},{mb,1,{},(struct R[]){{Ob,1,{{f,d},{l,C}}},{Sb,1,{{f,d},{l,C}}},{Nb,1,{{f,d},{l,C}}},{Tb,1,{{f,d},{l,C}}},{Pb,1,{{f,d},{l,C}}},{Ub,1,{{f,d},{l,C}}},{Wb,1,{{f,d},{l,C}}},{Qb,1,{{f,d},{l,C}}},}},{x},{mb,1,{},(struct R[]){{Ob,1,{{f,d},{l,ob}}},{Sb,1,{{f,d},{l,ob}}},{Nb,1,{{f,d},{l,ob}}},{Tb,1,{{f,d},{l,ob}}},{Pb,1,{{f,d},{l,ob}}},{Ub,1,{{f,d},{l,ob}}},{Wb,1,{{f,d},{l,ob}}},{Qb,1,{{f,d},{l,ob}}},}},{fc,1,{{f,b},{o,b}}},{fc,1,{{f,d},{o,d}}},{Vb,1,{{o,b},{f,b}}},{Vb,1,{{o,d},{f,d}}},{L,1,{{f,b},{o,b}}},{L,1,{{f,d},{o,d}}},{L,1,{{o,b},{f,b}}},{L,1,{{o,d},{f,d}}},{L,1,{{f,Cg},{be,O}}},{lg,1,{{o,d},{ae,d}}},{L,1,{{be,O},{f,O}}},{tb,1,{{f,U}}},{Rb},{Vb,0,{{Oc,d},{s,d}}},{Vb,0,{{Pc,d},{s,d}}},{Vb,0,{{Nc,d},{s,d}}},{Vb,0,{{dd,d},{s,d}}},{Vb,0,{{ad,d},{s,d}}},{Vb,0,{{cd,d},{s,d}}},{Vb,0,{{bd,d},{s,d}}},{ye},{dg},{x},{Q},{nb,0,{{Te,d}}},{tb,0,{{Te,d}}},{Gh},{Eh},{L,0,{{s,b},{wd,b}}},{L,0,{{s,d},{wd,d}}},{L,0,{{wd,b},{s,b}}},{L,0,{{wd,d},{s,d}}},{td,0,{{Fb,b},{gc,b}}},{td,0,{{Fb,d},{gc,d}}},{rd,0,{{Fb,b},{gc,b}}},{rd,0,{{Fb,d},{gc,d}}},{fc,0,{{s,b},{l,b}}},{fc,0,{{s,d},{l,C}}},{vd,0,{{Fb,b},{s,b}}},{vd,0,{{Fb,d},{s,d}}},{sd,0,{{s,b},{gc,b}}},{sd,0,{{s,d},{gc,d}}},{ud,0,{{Fb,b},{s,b}}},{ud,0,{{Fb,d},{s,d}}},{L,0,{{_c,b},{l,b}}},{L,0,{{Oc,b},{l,b}}},{L,0,{{Pc,b},{l,b}}},{L,0,{{Nc,b},{l,b}}},{L,0,{{Eg,b},{l,b}}},{L,0,{{Gg,b},{l,b}}},{L,0,{{Hg,b},{l,b}}},{L,0,{{Fg,b},{l,b}}},{L,0,{{_c,d},{l,d}}},{L,0,{{Oc,d},{l,d}}},{L,0,{{Pc,d},{l,d}}},{L,0,{{Nc,d},{l,d}}},{L,0,{{dd,d},{l,d}}},{L,0,{{ad,d},{l,d}}},{L,0,{{cd,d},{l,d}}},{L,0,{{bd,d},{l,d}}},{mb,1,{},(struct R[]){{Ic,1,{{f,b},{l,b}}},{Jc,1,{{f,b},{l,b}}},{Gc,1,{{f,b},{l,b}}},{Hc,1,{{f,b},{l,b}}},{ub,1,{{f,b},{l,b}}},{Lc,1,{{f,b},{l,b}}},{ub,1,{{f,b},{l,b}}},{Kc,1,{{f,b},{l,b}}},}},{mb,1,{},(struct R[]){{Ic,1,{{f,d},{l,b}}},{Jc,1,{{f,d},{l,b}}},{Gc,1,{{f,d},{l,b}}},{Hc,1,{{f,d},{l,b}}},{ub,1,{{f,d},{l,b}}},{Lc,1,{{f,d},{l,b}}},{ub,1,{{f,d},{l,b}}},{Kc,1,{{f,d},{l,b}}},}},{Wd,0,{{l,O}}},{Wd,0,},{x},{x},{L,1,{{f,b},{l,b}}},{L,1,{{f,d},{l,C}}},{eg,0,{{l,O},{l,b}}},{mg,0},{Vd,0,{{l,O}}},{Vd,0},{Td,0,{{Se,b}}},{Td,0,{{l,b}}},{x},{ig,0},{mb,1,{},(struct R[]){{Ic,1,{{f,b},{ab,b}}},{Jc,1,{{f,b},{ab,b}}},{Gc,1,{{f,b},{ab,b}}},{Hc,1,{{f,b},{ab,b}}},{ub,1,{{f,b},{ab,b}}},{Lc,1,{{f,b},{ab,b}}},{ub,1,{{f,b},{ab,b}}},{Kc,1,{{f,b},{ab,b}}},}},{mb,1,{},(struct R[]){{Ic,1,{{f,d},{ab,b}}},{Jc,1,{{f,d},{ab,b}}},{Gc,1,{{f,d},{ab,b}}},{Hc,1,{{f,d},{ab,b}}},{ub,1,{{f,d},{ab,b}}},{Lc,1,{{f,d},{ab,b}}},{ub,1,{{f,d},{ab,b}}},{Kc,1,{{f,d},{ab,b}}},}},{mb,1,{},(struct R[]){{Ic,1,{{f,b},{bb,b}}},{Jc,1,{{f,b},{bb,b}}},{Gc,1,{{f,b},{bb,b}}},{Hc,1,{{f,b},{bb,b}}},{ub,1,{{f,b},{bb,b}}},{Lc,1,{{f,b},{bb,b}}},{ub,1,{{f,b},{bb,b}}},{Kc,1,{{f,b},{bb,b}}},}},{mb,1,{},(struct R[]){{Ic,1,{{f,d},{bb,b}}},{Jc,1,{{f,d},{bb,b}}},{Gc,1,{{f,d},{bb,b}}},{Hc,1,{{f,d},{bb,b}}},{ub,1,{{f,d},{bb,b}}},{Lc,1,{{f,d},{bb,b}}},{ub,1,{{f,d},{bb,b}}},{Kc,1,{{f,d},{bb,b}}},}},{x},{x},{x},{xg,0},{uc,1},{uc,1},{uc,1},{uc,1},{uc,1},{uc,1},{uc,1},{uc,1},{Ge,0,{{D,ob}}},{Fe,0,{{D,ob}}},{ng,0,{{D,ob}}},{kg,0,{{D,ob},{bb,d}}},{Rd,0,{{s,b},{l,b}}},{Rd,0,{{s,Zc},{l,b}}},{Ud,0,{{l,b},{s,b}}},{Ud,0,{{l,b},{s,Zc}}},{ag,0,{{D,C}}},{Ee,0,{{D,C}}},{x},{Ee,0,{{D,ob}}},{Rd,0,{{s,b},{vb,O}}},{Rd,0,{{s,Zc},{vb,O}}},{Ud,0,{{vb,O},{s,b}}},{Ud,0,{{vb,O},{s,Zc}}},{Q},{Td,0,{{ab,b}}},{Q},{Q},{fg},{cg},{mb,1,{},(struct R[]){{fc,1,{{f,b},{l,b}}},{fc,1,{{f,b},{l,b}}},{Je,1,{{f,b}}},{Ie,1,{{f,b}}},{pg,1,{{s,O,.X=1},{s,b,.X=1},{f,b}}},{Ce,1,{{s,O,.X=1},{s,b,.X=1},{f,b}}},{Ae,1,{{s,b,.X=1},{Ue,b,.X=1},{s,O,.X=1},{f,b}}},{Be,1,{{s,b,.X=1},{Ue,b,.X=1},{s,O,.X=1},{f,b}}},}},{mb,1,{},(struct R[]){{fc,1,{{f,d},{l,C}}},{fc,1,{{f,d},{l,C}}},{Je,1,{{f,d}}},{Ie,1,{{f,d}}},{qg,1,{{s,d,.X=1},{vb,d,.X=1},{f,d}}},{gg,1,{{s,d,.X=1},{vb,d,.X=1},{f,d}}},{Ae,1,{{s,d,.X=1},{vb,d,.X=1},{s,d,.X=1},{f,d}}},{Be,1,{{s,d,.X=1},{vb,d,.X=1},{s,d,.X=1},{f,d}}},}},{ue},{tg},{we},{vg},{ve},{ug},{mb,1,{},(struct R[]){{Sd,1,{{f,b}}},{ze,1,{{f,b}}},}},{mb,1,{},(struct R[]){{Sd,1,{{f,d}}},{ze,1,{{f,d}}},{bg,1,{{f,Qe}}},{Bh,1,{{ae,U}}},{jg,1,{{f,Qe}}},{Dh,1,{{ae,U}}},{nb,1,{{f,U}}},{x},}},};const struct R rh[]={{h},{h},{h},{h},{h},{wg},{h},{h},{h},{h},{h},{h},{h},{Rb,1,{{f,_d}}},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{Rb},{Rb},{Rb},{Rb},{Rb},{Rb},{Rb,1},{h},{h},{h},{h},{h},{x},{h},{x},{h},{h},{h},{h},{h},{h},{h},{h},{h},{sg},{h},{h},{h},{h},{x},{h},{h},{x},{h},{x},{x},{x},{x},{x},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{db,1,{{o,d},{f,d}}},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{x},{x},{h},{h},{h},{h},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{K,0,{{D,C}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{eb,1,{{f,b}}},{nb,0,{{Jg,O}}},{tb,0,{{Jg,O}}},{Ch,0},{se,1,{{f,d},{o,d}}},{h},{h},{x},{x},{nb,0,{{Kg,O}}},{tb,0,{{Kg,O}}},{h},{te,1,{{f,d},{o,d}}},{h},{h},{h},{Ce,1,{{o,d},{o,d},{f,d}}},{xe,1,{{f,b},{s,b},{o,b}}},{xe,1,{{f,d},{s,d},{o,d}}},{h},{_f,1,{{f,d},{o,d}}},{h},{h},{L,1,{{o,d},{f,b}}},{L,1,{{o,d},{f,O}}},{h},{h},{mb,1,{},(struct R[]){{},{},{},{},{se,1,{{f,d},{l,b}}},{te,1,{{f,d},{l,b}}},{_f,1,{{f,d},{l,b}}},{Zf,1,{{f,d},{l,b}}},}},{Zf,1,{{f,d},{o,d}}},{Xf,1,{{o,d},{f,d}}},{Yf,1,{{o,d},{f,d}}},{He,1,{{o,d},{f,b}}},{He,1,{{o,d},{f,O}}},{Ke,1,{{f,b},{o,b}}},{Ke,1,{{f,d},{o,d}}},{h},{h},{h},{h},{h},{h},{tc,0,{{_c,b}}},{tc,0,{{Oc,b}}},{tc,0,{{Pc,b}}},{tc,0,{{Nc,b}}},{tc,0,{{dd,b}}},{tc,0,{{ad,b}}},{tc,0,{{cd,b}}},{tc,0,{{bd,b}}},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{h},{x},};enum Qm{ce=0,ki,ji,Og,};enum Rm{wb=(1<<0),xd=(1<<2),li=(1<<4),Db=(1<<6),hc=(1<<7),Gb=(1<<11),Dm=(1<<8),Pg=(1<<9),Xe=(1<<10),Cm=(1<<21),};enum lb{rc=-1,Ah=0,lj=1,kj=3,Mb=6,Uf=13,Vf=14,nj=16,mj};enum Um{rj=0,Xc,od,Pd};enum Tm{sj=0,Wf,Qd,};struct hj{i I;i y;i H,dh,_,Lm;i lc,Ad,Nm,Mm;i ie,Km,Fm,Gm,Hm,nf,Im,Jm;Tc ni,pi,N,si,wi,ke;i ti;i xi;};enum Wm{Fc=0,pd,qd,sc,ec};struct q;typedef int(*qh)(struct q*a,void*oc,i u,e*gd,e c,int fd,i*Yb);struct sb{union{i he;e*v;i u;};e J;e c;e mf;e cc;e Jb;e*Ze;e*sf;e eh;gb ib;i Cd;};struct q{struct hj j;i Gd;int xc;void*oc;qh df;qh ef;int Xb;int Sc;struct{int Hb;md Em;i I;i u;e je;}V;i yi;i ff;int Z;int wc;int hf;e vc;const struct R*oi;struct{e af;e Zb;e ac;e v;}E;struct{e af;e ke;e m;e v;}W;id ib;sb*g;int rb;int kc;i of;e hd;};const i hb=wb|xd|li|Db|hc|Gb;inline void kb(q*a,i Li,i Rg){a->j.y=(a->j.y&(~Rg))|(Li&Rg);}enum Om{jj=0,Ld,Mf,Ef,Of,Ff,qe,Tf,nd,Rf,Sf,Ec,xh,yh,vh,wh,Pf,Qf,Nf,zh,Lf,Od,Nd,pe,oe,Md,Kf,If,Jf,Gf,Hf,};const char*_i[]={"#DE - Divide error","#DB - Debug","#NMI","#BP - Breakpoint (INT 3)","#OF","#BR","#UD - Undefined opcode","#NM","#DF","#(Reserved)","#TS","#NP","#SS","#GP - General Protection","#PF - Page Fault"};const char*Zi(int qi){return _i[qi];}void lb(q*a,int ri){a->V.Hb=ri;a->V.I=a->ff;a->Xb=Og;a->xc=1;}void dc(q*a,i u,e*z,e c,int fd){int P;i Yb=0;if(a->df!=0){P=a->df(a,a->oc,u,z,c,fd,&Yb);}else{P=Qd;}if(P==Wf){lb(a,Uf);a->V.u=Yb;a->V.je=0;}else if(P==Qd){lb(a,Vf);a->V.u=Yb;a->V.je=0;}}void Id(q*a,i u,e*z,e c,int fd){int P;i Yb=0;if(a->ef!=0){P=a->ef(a,a->oc,u,z,c,fd,&Yb);}else{P=Qd;}if(P==Wf){lb(a,Uf);a->V.u=Yb;a->V.je=1;}else if(P==Qd){lb(a,Vf);a->V.u=Yb;a->V.je=1;}}e Vc(q*a){e r;dc(a,a->j.I,&r,1,Pd);a->j.I+=1;return r;}Tc bj(q*a){Tc r;dc(a,a->j.I,(e*)&r,2,Pd);a->j.I+=2;return r;}md le(q*a){md r;dc(a,a->j.I,(e*)&r,4,Pd);a->j.I+=4;return r;}i ph(q*a){i r;dc(a,a->j.I,(e*)&r,8,Pd);a->j.I+=8;return r;}void pc(q*a,int t,e*z,e c){a->g[t].J=pd;a->g[t].he=0;a->g[t].c=c;a->g[t].cc=0;memcpy(&a->g[t].he,z,c);a->g[t].Jb=0;}void Wc(q*a,int t,e*v,e c){a->g[t].J=qd;a->g[t].v=v;a->g[t].c=c;a->g[t].cc=0;a->g[t].Jb=0;}void fj(q*a,int t,e*v,e Ac,e c){a->g[t].J=sc;a->g[t].v=v;a->g[t].c=c;a->g[t].mf=Ac;a->g[t].cc=0;a->g[t].Jb=0;}void dj(q*a,int t,i u,e c){a->g[t].J=ec;a->g[t].u=u;a->g[t].c=c;a->g[t].cc=0;a->g[t].Jb=0;}void ej(q*a,int t,e*S,e*Ab,e _b,gb ib,e c){a->g[t].J=ec;a->g[t].u=0;a->g[t].c=c;a->g[t].cc=0;a->g[t].Jb=1;a->g[t].Ze=S;a->g[t].sf=Ab;a->g[t].eh=_b;a->g[t].ib=ib;}i me(q*a,int m){i r=0;sb*g=&a->g[m];if(g->J!=sc){{(*((char*)0))=1;};}switch(g->mf){case 4:r=*((md*)g->v);break;case 8:r=*((i*)g->v);break;default:{(*((char*)0))=1;};}return r;}void th(q*a,int t,int pb){sb*g=&a->g[t];if(g->J!=sc){return;}switch(g->mf){case 1:*((e*)g->v)+=pb;break;case 2:*((Tc*)g->v)+=pb;break;case 4:*((md*)g->v)+=pb;break;case 8:*((i*)g->v)+=pb;break;default:{(*((char*)0))=1;};}}void Jd(q*a,int m){sb*g=&a->g[m];i S=0,Ab=0;if(g->J!=ec||g->Jb==0){return;}if(g->Ze){memcpy(&S,g->Ze,8);}if(g->sf){memcpy(&Ab,g->sf,8);}g->u=S+(Ab*g->eh)+g->ib+g->Cd;g->Jb=0;}i cj(q*a,int m){Jd(a,m);return a->g[m].u;}void M(q*a,int m,e*jc,e c){sb*g=&a->g[m];i u;i z=0x00;switch(g->J){case pd:memcpy((e*)&z,&g->he,g->c);break;case qd:memcpy((e*)&z,g->v,g->c);break;case sc:u=me(a,m);dc(a,u,(e*)&z,c,Xc);break;case ec:if(g->Jb!=0){Jd(a,m);}u=g->u;dc(a,u,(e*)&z,c,Xc);break;default:{(*((char*)0))=1;};break;}memcpy(jc,(e*)&z,c);}void G(q*a,int m,i&jc,e c){sb*g=&a->g[m];i u;switch(g->J){case pd:jc=g->he;break;case qd:if(c==8)jc=*(i*)g->v;else if(c==4)jc=*(md*)g->v;else if(c==2)jc=*(Tc*)g->v;else jc=*g->v;break;case sc:u=me(a,m);dc(a,u,(e*)&jc,c,Xc);break;case ec:if(g->Jb!=0){Jd(a,m);}u=g->u;dc(a,u,(e*)&jc,c,Xc);break;default:{(*((char*)0))=1;};break;}}void Lb(q*a,int m,e*Ib,e c){sb*g=&a->g[m];i u;switch(g->J){case pd:break;case qd:if(g->c==4){i z=0;memcpy((e*)&z,Ib,c);memcpy(g->v,(e*)&z,8);}else{memcpy(g->v,Ib,g->c);}break;case sc:u=me(a,m);Id(a,u,Ib,c,od);break;case ec:if(g->Jb!=0){Jd(a,m);}u=g->u;Id(a,u,Ib,c,od);break;default:{(*((char*)0))=1;};break;}}void T(q*a,int m,i Ib,e c){sb*g=&a->g[m];i u;switch(g->J){case pd:break;case qd:if(c==8)*(i*)g->v=Ib;else if(c==4)*(i*)g->v=(md)Ib;else if(c==2)*(Tc*)g->v=Ib;else if(c==1)*g->v=Ib;break;case sc:u=me(a,m);Id(a,u,(e*)&Ib,c,od);break;case ec:if(g->Jb!=0){Jd(a,m);}u=g->u;Id(a,u,(e*)&Ib,c,od);break;default:{(*((char*)0))=1;};break;}}void Kd(q*a,int t,e yb,int cc){sb*g=&a->g[t];i F=0;if(yb<=g->c){{(*((char*)0))=1;};return;}if(g->J==sc){{(*((char*)0))=1;};return;}if(cc==0){i F=0x00;G(a,t,F,g->c);pc(a,t,(e*)&F,yb);return;}switch(g->c){case 1:{bf n;M(a,t,(e*)&n,1);F=(bf)n;}break;case 2:{Zg n;M(a,t,(e*)&n,2);F=(Zg)n;}break;case 4:{id n;M(a,t,(e*)&n,4);F=(id)n;}break;}pc(a,t,(e*)&F,yb);}void Dc(q*a,int ld,int rf,e v,e c,e**r,e*pf){e*g=0;e Kb=8;int Vg=1;int ih=0;{if(!(v>=0&&v<=7)){fprintf(stderr,"Assertion failed at %s:%d in %s. Condition: ""reg >= 0 && reg <= 7""\n","cpu.h",695,__PRETTY_FUNCTION__);{(*((char*)0))=1;};}};{if(!(c==1||c==2||c==4||c==8||c==10||c==16)){fprintf(stderr,"Assertion failed at %s:%d in %s. Condition: ""size == 1 || size == 2 || size == 4 || size == 8 || size == 10 || size == 16""\n","cpu.h",697,__PRETTY_FUNCTION__);{(*((char*)0))=1;};}};if(((a->Z&Ne)!=0)||(c>1)){Vg=0;}if((a->Z&rf)!=0){ih=1;}switch(ld){case Cb:case ed:if(ld==ed&&Vg==1){if(v<4){g=(e*)&a->j.H+8*v;}else{g=(e*)&a->j.H+8*v-32+1;}Kb=1;}else if(ih!=0){g=(e*)&a->j.ie+8*v;}else{g=(e*)&a->j.H+8*v;}break;case ii:g=(e*)&a->j.y;break;case Ng:Kb=2;switch(v){case 0x00:g=(e*)&a->j.N;break;case 0x01:g=(e*)&a->j.ni;break;case 0x02:g=(e*)&a->j.ke;break;case 0x03:g=(e*)&a->j.pi;break;case 0x04:g=(e*)&a->j.si;break;case 0x05:g=(e*)&a->j.wi;break;default:lb(a,Mb);break;}break;default:{(*((char*)0))=1;};break;}{if(!(c<=Kb)){fprintf(stderr,"Assertion failed at %s:%d in %s. Condition: ""size <= op_size""\n","cpu.h",769,__PRETTY_FUNCTION__);{(*((char*)0))=1;};}};Kb=c;(*r)=g;(*pf)=Kb;}void Eb(q*a,int m,int ld,int rf,e v,e c){e*g=0,jb=0;Dc(a,ld,rf,v,c,&g,&jb);Wc(a,m,g,jb);}void Wi(q*a,int J,int c,e*pf,e*Ki){int r=0;int Cc=0;int kd=a->Z;switch(J){case Ig:case Dg:r=10;break;case ab:case Se:r=1;break;case be:r=2;break;default:switch(c){case b:r=1;break;case ob:r=1;Cc=1;break;case Pe:r=1;Cc=1;break;case O:r=2;break;case Zc:r=4;break;case Qe:r=8;break;case Bg:if((kd&Zd)!=0){r=8;}else{r=4;}break;case _d:if((kd&Yc)!=0){r=2;}else{r=4;}break;case Re:case C:if((kd&Yc)!=0){r=2;}else{r=4;}Cc=1;break;case U:if((kd&Yc)!=0){r=2;}else{r=8;}break;case Cg:case d:if((kd&Zd)!=0){r=8;}else if((kd&Yc)!=0){r=2;}else{r=4;}break;case Uh:r=2;break;case Kh:r=4;break;case Qh:r=8;break;case Lh:r=16;break;case Jh:r=10;break;case Sh:r=4;break;case Mh:r=8;break;case Nh:r=10;break;case Rh:r=16;break;case Oh:r=16;break;case Th:r=16;break;case Ph:r=16;break;default:{(*((char*)0))=1;};break;}break;}(*pf)=r;(*Ki)=Cc;}e zf(q*a){e r=8;if((a->Z&Le)==1){r=4;}return r;}void oh(q*a,e**Fi,e**Hi,e*Gi){e _b=1;e*Ab=0,*S=0;switch(a->W.ke){case 0x01:_b=2;break;case 0x02:_b=4;break;case 0x03:_b=8;break;}if((a->Z&Ag)==0){if(a->W.m!=0x04)Ab=(e*)&a->j.H+8*a->W.m;else Ab=(e*)0;}else{Ab=(e*)&a->j.ie+8*a->W.m;}if((a->Z&Mc)==0){if(a->W.v==0x05)switch(a->E.Zb){case 0x00:break;case 0x01:S=(e*)&a->j.Ad;break;case 0x02:S=(e*)&a->j.Ad;break;}else S=(e*)&a->j.H+8*a->W.v;}else{if(a->W.v==0x05)switch(a->E.Zb){case 0x00:break;case 0x01:S=(e*)&a->j.nf;break;case 0x10:S=(e*)&a->j.nf;break;}else S=(e*)&a->j.ie+8*a->W.v;}(*Fi)=S;(*Hi)=Ab;(*Gi)=_b;}void Af(q*a,int m,int ld,e c){e*g=0;e Kb=0;int _g=0;e*S=0,*Ab=0;e _b=1;switch(a->E.Zb){case 0x00:case 0x01:case 0x02:_g=1;Kb=c;if((a->Z&Mc)==0){switch(a->E.ac){case 0x04:oh(a,&S,&Ab,&_b);break;case 0x05:if(a->E.Zb==0x00){S=(e*)&a->j.I;}else{S=(e*)&a->j.Ad;}break;default:S=(e*)&a->j.H+8*a->E.ac;}}else{switch(a->E.ac){case 0x04:oh(a,&S,&Ab,&_b);break;case 0x05:if(a->E.Zb==0x00){S=(e*)&a->j.I;}else{S=(e*)&a->j.nf;}break;default:S=(e*)&a->j.ie+8*a->E.ac;}}break;case 0x03:Dc(a,ld,Mc,a->E.ac,c,&g,&Kb);break;}if(Kb==0){lb(a,Mb);}else if(_g){a->g[m].Cd=0;ej(a,m,S,Ab,_b,a->ib,Kb);if((a->Z&Xd)){a->g[m].Cd=a->j.ti;}else if((a->Z&Yd)){a->g[m].Cd=a->j.xi;}else{a->g[m].Cd=0;}}else{Wc(a,m,g,Kb);}}void kh(q*a,int m,e c){Af(a,m,ed,c);}void Qi(q*a,int m,e c){e*g=0,jb=0;Dc(a,ed,Oe,a->E.v,c,&g,&jb);Wc(a,m,g,jb);}void Si(q*a,int m,e c){e*g=0,jb=0;Dc(a,Ng,0,a->E.v,c,&g,&jb);Wc(a,m,g,jb);}void Pi(q*a,int m,e c){Wc(a,m,(e*)&a->j.y,c);}void mh(q*a,int m,e c){i F=0;switch(c){case 1:F=Vc(a);break;case 2:F=bj(a);break;case 4:F=le(a);break;case 8:F=ph(a);break;default:{(*((char*)0))=1;};break;}pc(a,m,(e*)&F,c);}void Ri(q*a,int m,e c){i F=0;switch(zf(a)){case 4:F=le(a);break;case 8:F=ph(a);break;default:{(*((char*)0))=1;};break;}dj(a,m,F,c);}void nh(q*a,int m,e zi,e c){e*kf=0;e lf=0;e Ye;Ye=zf(a);if(zi){Dc(a,Cb,0,fi,Ye,&kf,&lf);}else{Dc(a,Cb,0,hi,Ye,&kf,&lf);}fj(a,m,kf,lf,c);}void Ti(q*a,int m,e c){e*g=0,jb=0;{if(!(c==16)){fprintf(stderr,"Assertion failed at %s:%d in %s. Condition: ""size == 16""\n","cpu.h",1170,__PRETTY_FUNCTION__);{(*((char*)0))=1;};}};Dc(a,We,Mc,a->E.ac,c,&g,&jb);Wc(a,m,g,jb);}void Ui(q*a,int m,e c){e*g=0,jb=0;{if(!(c==16)){fprintf(stderr,"Assertion failed at %s:%d in %s. Condition: ""size == 16""\n","cpu.h",1181,__PRETTY_FUNCTION__);{(*((char*)0))=1;};}};Dc(a,We,Oe,a->E.v,c,&g,&jb);Wc(a,m,g,jb);}void Vi(q*a,int m,e c){{if(!(c==16)){fprintf(stderr,"Assertion failed at %s:%d in %s. Condition: ""size == 16""\n","cpu.h",1190,__PRETTY_FUNCTION__);{(*((char*)0))=1;};}};Af(a,m,We,c);}void lh(q*a,int m,e c){Af(a,m,Mg,c);}void Xi(q*a,int m,int J,int Wg){i F=0;e c=0;e Cc=0;Wi(a,J,Wg,&c,&Cc);switch(J){case ab:F=1;pc(a,m,(e*)&F,1);break;case Se:F=3;pc(a,m,(e*)&F,1);break;case f:kh(a,m,c);break;case o:Qi(a,m,c);break;case be:Si(a,m,c);break;case Te:Pi(a,m,c);break;case ae:kh(a,m,c);if(a->g[m].J!=ec){a->g[m].J=Fc;lb(a,Mb);}break;case l:mh(a,m,c);break;case D:mh(a,m,c);Kd(a,m,8,1);break;case wd:Ri(a,m,c);break;case gc:nh(a,m,0,c);break;case Fb:nh(a,m,1,c);break;case s:case bb:case vb:case Yh:case ai:case Xh:case _h:case Zh:Eb(a,m,Cb,0,(J&0x07),c);break;case _c:case Oc:case Pc:case Nc:case dd:case ad:case cd:case bd:Eb(a,m,Cb,Mc,(J&0x07),c);break;case Eg:case Gg:case Hg:case Fg:Eb(a,m,ed,Mc,(J&0x07),c);break;case Ue:Eb(a,m,ed,0,(J&0x07),c);break;case Ig:Eb(a,m,Mg,0,(0),10);break;case Vh:lh(a,m,c);break;case Dg:lh(a,m,c);if(a->g[m].J==ec){a->g[m].J=Fc;lb(a,Mb);}break;case bi:Ti(a,m,c);break;case ci:Ui(a,m,c);break;case di:Vi(a,m,c);break;default:{(*((char*)0))=1;};break;}if(Cc){a->g[m].cc=Cc;switch((int)Wg){case Pe:case Re:Kd(a,m,8,1);a->g[m].cc=0;break;}}}int Yi(q*a,e B){int k=0x00;if(((B&0xF0)==0x40)&&!(a->Z&Ne)){k|=Ne;if(B&0x01){k|=Mc;}if(B&0x02){k|=Ag;}if(B&0x04){k|=Oe;}if(B&0x08){k|=Zd;}}else if(B==0x66){k|=Yc;}else if(B==0x67&&!(a->Z&Le)){k|=Le;}else if(((B&0xFE)==0xF2)&&(a->rb==0)){a->rb=(B&0x01)?Ih:Me;a->of=a->j.I;k|=a->rb;}else if(B==0x64&&((k&Xd)==0||((k&Yd)==0))){k|=Xd;}else if(B==0x65&&((k&Xd)==0||((k&Yd)==0))){k|=Yd;}else if(B==0xF0&&((k&zg)==0)){k|=zg;}else if(B==0x9B&&((k&yg)==0)){k|=yg;}else{switch(B){case 0x26:case 0x2E:case 0x36:case 0x3E:k|=Hh;break;}}a->Z|=k;return k;}void Mi(q*a,e B){e E;E=Vc(a);a->E.af=E;a->E.Zb=(E&0xC0)>>6;a->E.ac=(E&0x07);a->E.v=(E&0x38)>>3;if(a->E.Zb!=0x03&&a->E.ac==0x04){e Ed;Ed=Vc(a);a->W.af=Ed;a->W.ke=(Ed&0xC0)>>6;a->W.m=(Ed&0x38)>>3;a->W.v=(Ed&0x07);}a->ib=0x00;if(a->E.Zb==0x02){a->ib=(id)le(a);}else if(a->E.Zb==0x01){a->ib=(bf)Vc(a);}else if(a->E.Zb==0x00){if(a->E.ac==0x05||(a->E.ac==0x04&&a->W.v==0x05)){a->ib=(id)le(a);}}}int jh(q*a,e B,const R*zb){int Y;if(zb->Ci){Mi(a,B);}a->hf=re;while(1){if(zb->cb==mb){zb=&(zb->vi[a->E.v]);}else{break;}}for(Y=0;Y<4;Y++){if(zb->jf[Y].J==Wh){break;}int Ei=zb->jf[Y].J;int jb=zb->jf[Y].c;Xi(a,Y,Ei,jb);if(a->V.Hb!=rc){return x;}}a->oi=zb;return zb->cb;}int Oi(q*a,e B){const R zb=sh[B];return jh(a,B,&zb);}int Ni(q*a,e B){const R zb=rh[B];return jh(a,B,&zb);}void Bb(i&k,bool z){if(z)k|=wb;else k&=~wb;}void tf(i&k,bool z){if(z)k|=xd;else k&=~xd;}void Dd(i&k,bool z){if(z)k|=Db;else k&=~Db;}void uf(i&k,bool z){if(z)k|=hc;else k&=~hc;}void mc(i&k,bool z){if(z)k|=Gb;else k&=~Gb;}bool cf(i k){return k&wb;}void hh(i&k,i n,i p,i&P,e c){i xb=c*8-1,ic;bool bc=n>>xb;bool Bc=p>>xb;bool nc=(P>>xb)&1;if(c<8){Bb(k,P>>xb+1);ic=(1ull<<xb+1)-1;}else{Bb(k,(bc==Bc&&bc)||(bc!=Bc&&!nc));ic=~0ull;}tf(k,1^__builtin_parityll(P&255));Dd(k,(P&ic)==0);uf(k,nc);mc(k,bc==Bc&&bc!=nc);P&=ic;}void wf(i&k,i n,i p,i&P,e c){i xb=c*8-1,ic;bool bc=n>>xb;bool Bc=p>>xb;bool nc=(P>>xb)&1;if(c<8){Bb(k,P>>xb+1);ic=(1ull<<xb+1)-1;}else{Bb(k,(bc==Bc&&nc)||(bc!=Bc&&Bc));ic=~0ull;}tf(k,1^__builtin_parityll(P&255));Dd(k,(P&ic)==0);uf(k,nc);mc(k,bc!=Bc&&bc!=nc);P&=ic;}void Uc(i&k,i P,e c){i xb=c*8-1;bool nc=P>>xb;tf(k,__builtin_parity(P&255));Dd(k,P==0);uf(k,nc);}void A(q*a,int g){i k=a->j.y;e c=0;if(a->g[1].J!=Fc){if(a->g[0].c!=a->g[1].c){Kd(a,1,a->g[0].c,a->g[1].cc);if(a->g[0].c!=a->g[1].c){{(*((char*)0))=1;};}}}c=a->g[0].c;switch(g){case Ld:{i n=0,p=0,w;G(a,0,n,c);G(a,1,p,c);w=n+p;hh(k,n,p,w,c);kb(a,k,hb);T(a,0,w,c);}break;case Mf:{i n=0,p=0,w;G(a,0,n,c);G(a,1,p,c);w=n|p;mc(k,0);Bb(k,0);Uc(k,w,c);kb(a,k,hb);T(a,0,w,c);}break;case Ef:{i n=0,p=0,w;G(a,0,n,c);G(a,1,p,c);w=n+p+cf(k);hh(k,n,p,w,c);kb(a,k,hb);T(a,0,w,c);}break;case Of:{i n=0,p=0,w;G(a,0,n,c);G(a,1,p,c);w=n-p-cf(k);wf(k,n,p,w,c);kb(a,k,hb);T(a,0,w,c);}break;case Ff:{i n=0,p=0,w;G(a,0,n,c);G(a,1,p,c);w=n&p;mc(k,0);Bb(k,0);Uc(k,w,c);kb(a,k,hb);T(a,0,w,c);}break;case qe:{i n=0,p=0,w;G(a,0,n,c);G(a,1,p,c);w=n-p;wf(k,n,p,w,c);kb(a,k,hb);T(a,0,w,c);}break;case Tf:{i n=0,p=0,w;G(a,0,n,c);G(a,1,p,c);w=n^p;mc(k,0);Bb(k,0);Uc(k,w,c);kb(a,k,hb);T(a,0,w,c);}break;case nd:{i n=0,p=0,w;G(a,0,n,c);G(a,1,p,c);w=n-p;wf(k,n,p,w,c);kb(a,k,hb);}break;case Rf:{i n=0,p=0,w;G(a,0,n,c);G(a,1,p,c);w=n&p;mc(k,0);Bb(k,0);Uc(k,w,c);kb(a,k,hb);}break;case Sf:{i n=0,p=0;G(a,0,n,c);G(a,1,p,c);T(a,1,n,c);T(a,0,p,c);}break;case Ec:{i z=0;G(a,1,z,c);T(a,0,z,c);}break;case Pf:case zh:{i n=0,w;e p;G(a,0,n,c);M(a,1,(e*)&p,1);w=n<<p;Bb(k,n<<p-1>>c*8-1);if(p==1)mc(k,(w>>c*8-1)^cf(k));Uc(k,w,c);kb(a,k,hb);T(a,0,w,c);}break;case Qf:{i n=0,w;e p;G(a,0,n,c);M(a,1,(e*)&p,1);w=n>>p;Bb(k,n>>p-1&1);if(p==1)mc(k,0);Uc(k,w,c);kb(a,k,hb);T(a,0,w,c);}break;case Nf:{i n=0,w;e p;G(a,0,n,c);M(a,1,(e*)&p,1);if(n>>c*8-1)w=(n>>p)|((1ull<<p)-1)<<c*8-p;else w=n>>p;Bb(k,n>>p-1&1);if(p==1)mc(k,0);Uc(k,w,c);kb(a,k,hb);T(a,0,w,c);}break;case Lf:{i pb;G(a,0,pb,c);pb=~pb;T(a,0,pb,c);}break;case Od:case pe:case Nd:case oe:{i H=0,_=0,qb=0;int zd=0;G(a,0,H,c);G(a,2,qb,c);if(g==Od||g==Nd){__uint128_t Bd=(__uint128_t)H*(__uint128_t)qb;H=((i*)&Bd)[0];_=((i*)&Bd)[0];}else{__int128_t Bd=(__int128_t)(gb)H*(__int128_t)(gb)qb;H=((i*)&Bd)[0];_=((i*)&Bd)[0];}switch(c){case 1:if((H&0xff00)!=0){zd=1;}break;case 2:_=(H&0xffff0000)>>16;H=(H&0x0000ffff);if(_){zd=1;}break;case 4:_=(H&0xffffffff00000000)>>32;H=(H&0x00000000ffffffff);if(_){zd=1;}break;case 8:if(_){zd=1;}break;}if(zd){a->j.y|=(wb|Gb);}else{a->j.y&=~(wb|Gb);}if(c==1){T(a,0,H,2);}else{T(a,0,H,c);if(g==Od||g==pe){T(a,1,_,c);}}}break;case Md:case Kf:{i H=0,_=0,qb=0;if(c>1){G(a,1,_,c);}G(a,2,H,c);G(a,3,qb,c);if(qb==0){lb(a,Ah);break;}if(_){if(g==Md){__uint128_t z=(__uint128_t)_<<64|(__uint128_t)H;__uint128_t Qc=z/qb,Rc=z%qb;H=((i*)&Qc)[0];_=Rc;}else{__int128_t z=(__uint128_t)_<<64|(__uint128_t)H;__int128_t Qc=z/(gb)qb,Rc=z%(gb)qb;H=((i*)&Qc)[0];_=Rc;}}else{if(g==Md){i Qc=H/qb,Rc=H%qb;H=Qc;_=Rc;}else{i Qc=(gb)H/(gb)qb,Rc=(gb)H%(gb)qb;H=Qc;_=Rc;}}T(a,0,H,c);T(a,1,_,c);}break;case If:{i n=0,p=0;G(a,0,n,c);G(a,1,p,c);Bb(k,n>>(p&c*8-1)&1);kb(a,k,hb);}break;case Jf:{i n=0,p=0;G(a,0,n,c);G(a,1,p,c);p&=c*8-1;Bb(k,n>>p&1);n|=1ull<<p;kb(a,k,hb);T(a,0,n,c);}break;case Gf:{i n=0,p=0;G(a,1,p,c);n=__builtin_ctzll(p);Dd(k,n==0);kb(a,k,hb);T(a,0,n,c);}break;case Hf:{i n=0,p=0;G(a,1,p,c);n=c*8-1-__builtin_clzll(p);Dd(k,n==0);kb(a,k,hb);T(a,0,n,c);}break;default:lb(a,Mb);break;}}void ne(q*a,int t){i pb=0x00;M(a,t,(e*)&pb,a->g[t].c);a->j.lc-=8;Id(a,a->j.lc,(e*)&pb,8,od);}void Df(q*a,int t){i pb=0x00;dc(a,a->j.lc,(e*)&pb,8,Xc);a->j.lc+=8;Lb(a,t,(e*)&pb,a->g[t].c);}int xf(q*a,e B){int fb=0;switch(B&0x0F){case 0x00:fb=((a->j.y&Gb)!=0);break;case 0x01:fb=((a->j.y&Gb)==0);break;case 0x02:fb=((a->j.y&wb)!=0);break;case 0x03:fb=((a->j.y&wb)==0);break;case 0x04:fb=((a->j.y&Db)!=0);break;case 0x05:fb=((a->j.y&Db)==0);break;case 0x06:fb=((a->j.y&wb)!=0||(a->j.y&Db)!=0);break;case 0x07:fb=((a->j.y&wb)==0&&(a->j.y&Db)==0);break;case 0x08:fb=((a->j.y&hc)!=0);break;case 0x09:fb=((a->j.y&hc)==0);break;case 0x0A:fb=((a->j.y&xd)!=0);break;case 0x0B:fb=((a->j.y&xd)==0);break;case 0x0C:fb=(((a->j.y&hc)?1:0)!=((a->j.y&Gb)?1:0));break;case 0x0D:fb=(((a->j.y&hc)?1:0)==((a->j.y&Gb)?1:0));break;case 0x0E:fb=(((a->j.y&Db)?1:0)==1)||(((a->j.y&hc)?1:0)!=((a->j.y&Gb)?1:0));break;case 0x0F:fb=(((a->j.y&Db)?1:0)==0)&&(((a->j.y&hc)?1:0)==((a->j.y&Gb)?1:0));break;}return fb;}int Cf(q*a,int Fd,int yc){int r=1;a->j.dh-=1;if(a->j.dh==0){r=0;}if(Fd){if(yc){if((a->j.y&Db)!=0){r=0;}}else{if((a->j.y&Db)==0){r=0;}}}return r;}void Hd(q*a,int t){gb jd=0;M(a,t,(e*)&jd,a->g[t].c);switch(a->g[t].c){case 1:{bf F;M(a,t,(e*)&F,1);jd=(gb)F;}break;case 2:{Zg F;M(a,t,(e*)&F,2);jd=(gb)F;}break;case 4:{id F;M(a,t,(e*)&F,4);jd=(gb)F;}break;case 8:{gb F;M(a,t,(e*)&F,8);jd=(gb)F;}break;default:{(*((char*)0))=1;};break;}a->j.I+=(gb)jd;}void gj(q*a,int cb,e B){int zc=1;switch(a->g[0].c){case 1:zc=1;break;case 2:zc=2;break;case 4:zc=4;break;case 8:zc=8;break;}if(a->j.y&Xe){zc=-zc;}switch(cb){case td:A(a,Ec);break;case rd:A(a,nd);break;case ud:A(a,nd);break;case vd:A(a,Ec);break;case sd:A(a,Ec);break;default:break;}th(a,0,zc);th(a,1,zc);}void uh(q*a,int cb,e B){if(a->rb!=0){switch(cb){case td:case vd:case sd:break;case rd:case ud:a->kc=1;break;case ig:case Wd:case Vd:case K:a->rb=0;break;default:lb(a,Mb);return;break;}}switch(cb){case Rb:break;case Ob:A(a,Ld);break;case Nb:A(a,Ef);break;case Pb:A(a,Ff);break;case Wb:A(a,Tf);break;case Sb:A(a,Mf);break;case Tb:A(a,Of);break;case Ub:A(a,qe);break;case Qb:A(a,nd);break;case fc:A(a,Rf);break;case Vb:A(a,Sf);break;case Je:A(a,Lf);break;case Ie:{i F=0;M(a,0,(e*)&F,a->g[0].c);F=(0-F);Lb(a,0,(e*)&F,a->g[0].c);}break;case L:A(a,Ec);break;case lg:{i mi=cj(a,1);Lb(a,0,(e*)&mi,a->g[0].c);}break;case He:case og:Kd(a,1,a->g[0].c,1);A(a,Ec);break;case td:case rd:case ud:case vd:case sd:gj(a,cb,B);break;case db:if(xf(a,B)){A(a,Ec);}break;case nb:ne(a,0);break;case tb:Df(a,0);break;case Ee:Hd(a,0);break;case K:if(xf(a,B)){Hd(a,0);}break;case ng:case Fe:case Ge:{int Fd=0,yc=0;switch((int)cb){case Fe:Fd=1;break;case Ge:Fd=1;yc=1;break;}if(Cf(a,Fd,yc)==1){Hd(a,0);}}break;case kg:{i Yg=0;M(a,1,(e*)&Yg,8);if(Yg==0){Hd(a,0);}}break;case pg:A(a,Nd);break;case qg:A(a,Od);break;case Ce:A(a,oe);break;case gg:A(a,pe);break;case Fh:{i z=0;M(a,1,(e*)&z,a->g[1].c);Lb(a,0,(e*)&z,a->g[0].c);A(a,Nd);}break;case De:{i z=0;M(a,1,(e*)&z,a->g[1].c);Lb(a,0,(e*)&z,a->g[0].c);A(a,oe);}break;case Ae:A(a,Md);break;case Be:A(a,Kf);break;case Sd:case ze:{i z=1;pc(a,1,(e*)&z,1);if(cb==Sd){A(a,Ld);}else{A(a,qe);}}break;case ag:{pc(a,3,(e*)&a->j.I,8);ne(a,3);Hd(a,0);}break;case bg:{i u=0;M(a,0,(e*)&u,a->g[0].c);pc(a,3,(e*)&a->j.I,8);ne(a,3);a->j.I=u;}break;case jg:{i u=0;M(a,0,(e*)&u,a->g[0].c);a->j.I=u;}break;case Wd:case Vd:{i qf=0;pc(a,3,(e*)&qf,8);Df(a,3);M(a,3,(e*)&qf,8);if(a->g[0].J>0){Tc Sg=0;M(a,0,(e*)&Sg,2);a->j.lc+=Sg;}a->j.I=qf;}break;case eg:{Eb(a,3,Cb,0,Lg,8);ne(a,3);a->j.Ad=a->j.lc;Tc ah=0;M(a,0,(e*)&ah,2);a->j.lc-=ah;}break;case mg:{a->j.lc=a->j.Ad;Eb(a,3,Cb,0,Lg,8);Df(a,3);}break;case Td:{e Sc=0;M(a,0,(e*)&Sc,1);a->Sc=Sc;a->Xb=ji;a->xc=1;}break;case wg:{a->Xb=ki;}break;case xg:{i u=0;e de=0;e Qg=zf(a);Eb(a,3,Cb,0,ei,Qg);Eb(a,4,Cb,0,Ve,1);M(a,4,(e*)&de,1);M(a,3,(e*)&u,Qg);u+=de;dc(a,u,(e*)&de,1,Xc);Lb(a,4,(e*)&de,1);}break;case Ic:A(a,xh);break;case Jc:A(a,yh);break;case Gc:A(a,vh);break;case Hc:A(a,wh);break;case ub:A(a,Pf);break;case Lc:A(a,Qf);break;case Kc:A(a,Nf);break;case cg:a->j.y^=wb;break;case ue:case tg:if(cb==ue){a->j.y&=~wb;}else{a->j.y|=wb;}break;case we:case vg:if(cb==we){a->j.y&=~Pg;}else{a->j.y|=Pg;}break;case ve:case ug:if(cb==ve){a->j.y&=~Xe;}else{a->j.y|=Xe;}break;case ye:case dg:{int Ac=0,yb=0;if(a->Z&Zd){Ac=4;yb=8;}else if(a->Z&Yc){Ac=1;yb=2;}else{Ac=2;yb=4;}if(cb==ye){Eb(a,0,Cb,0,Ve,Ac);Kd(a,0,yb,1);}else{i n=0,p=0;Eb(a,0,Cb,0,Ve,Ac);Eb(a,1,Cb,0,gi,Ac);M(a,0,(e*)&n,yb);if(yb==2){p=(n&0x8000)?-1:0;}else if(yb==4){p=(n&0x80000000)?-1:0;}else if(yb==8){p=(n&0x8000000000000000)?-1:0;}Lb(a,1,(e*)&p,yb);}}break;case eb:{e z=xf(a,B);Lb(a,0,(e*)&z,1);}break;case se:A(a,If);break;case te:A(a,Jf);break;case Xf:A(a,Gf);break;case Yf:A(a,Hf);break;case xe:{i fh=0,Xg=0;M(a,0,(e*)&Xg,a->g[0].c);M(a,2,(e*)&fh,a->g[2].c);A(a,nd);if((a->j.y&Db)!=0){Lb(a,0,(e*)&fh,a->g[0].c);}else{Lb(a,1,(e*)&Xg,a->g[1].c);}}break;case Ke:{i bh=0;M(a,0,(e*)&bh,a->g[0].c);A(a,Ld);Lb(a,1,(e*)&bh,a->g[1].c);}break;case fg:a->xc=1;break;case sg:a->j.H=(a->Gd&0xffffffff);a->j._=(a->Gd>>32);break;case x:lb(a,Mb);break;default:fprintf(stderr,"[-] CPU: opcode %02x not implemented\n",B);lb(a,Mb);break;}}void Bf(q*a){a->ff=a->j.I;a->V.Hb=rc;a->Sc=-1;a->Z=0;memset(&a->E,0,sizeof(a->E));memset(&a->W,0,sizeof(a->W));a->ib=0;a->g[0].J=Fc;a->g[1].J=Fc;a->g[2].J=Fc;a->g[3].J=Fc;a->hd=0;a->kc=0;}int yf(q*a){int r=-1;e B;int cb=0x00;B=Vc(a);if(a->V.Hb!=rc){goto _end;}while(Yi(a,B)){B=Vc(a);if(a->V.Hb!=rc){goto _end;}}if(B==0x0F){B=Vc(a);if(a->V.Hb!=rc){goto _end;}cb=Ni(a,B);}else{cb=Oi(a,B);}a->wc=cb;a->vc=B;r=0;_end:return r;}int Pm(q*a){if(a->xc){return 0;}a->Xb=ce;Bf(a);yf(a);if(a->V.Hb!=rc){goto _end;}if(a->hf==re){uh(a,a->wc,a->vc);}else{lb(a,Mb);}_end:a->yi+=1;if(a->Xb!=ce){a->xc=1;a->rb=0;}if(a->rb!=0x00){int yc=(a->rb==Me)?1:0;if(Cf(a,a->kc,yc)!=0){a->j.I=a->of;}else{a->rb=0;}}a->Gd+=26;return a->Xb;}void ij(q*a){a->j.I=0x00;a->j.y=0x00;a->V.Hb=rc;a->Sc=-1;a->rb=0;a->xc=0;a->Gd=0;}namespace no_memory_change_eval{const i vf=8192;e gh[vf];int Ai(q*a,void*oc,i u,e*gd,e c,int fd,i*Yb){if(u==40){memset(gd,0,c);return 0;}std::map<i,e>&map=*(std::map<i,e>*)oc;for(e Y=0;Y<c;Y++){if(map.count(u+Y)){gd[Y]=map[u+Y];}else{gd[Y]=reinterpret_cast<e*>(u)[Y];}}return 0;}int Bi(q*a,void*oc,i u,e*gd,e c,int fd,i*Yb){std::map<i,e>&map=*(std::map<i,e>*)oc;for(e Y=0;Y<c;Y++){map[u+Y]=gd[Y];}return 0;}const int qc=10000;sb yd[4],*ch;int gf=0;struct fe{i I;int Z;id ib;int kc;int wc;int vc;e hd;__typeof(q::E)E;__typeof(q::W)W;sb*g;}*Ug;int Tg=0;fe**ge;i _e;void Ji(i Di){_e=Di;memset(ge,0,qc*16);gf=0;Tg=0;}int aj(q*a){if(a->xc){return 0;}a->Xb=ce;gb ee=a->j.I-_e;if(ee<-qc||ee>=qc){a->g=yd;Bf(a);yf(a);}else{fe*N=ge[ee+qc];if(N){a->ff=a->j.I;a->V.Hb=rc;a->Sc=-1;a->j.I=N->I;a->E=N->E;a->W=N->W;a->Z=N->Z;a->ib=N->ib;a->kc=N->kc;a->hd=N->hd;a->wc=N->wc;a->vc=N->vc;a->g=yd;memcpy(yd,N->g,sizeof(sb)*4);}else{a->g=ch+gf;gf+=4;Bf(a);yf(a);N=&Ug[Tg++];N->I=a->j.I;N->E=a->E;N->W=a->W;N->Z=a->Z;N->ib=a->ib;N->kc=a->kc;N->hd=a->hd;N->wc=a->wc;N->vc=a->vc;N->g=a->g;a->g=yd;memcpy(yd,N->g,sizeof(sb)*4);ge[ee+qc]=N;}}if(a->hf==re){uh(a,a->wc,a->vc);}else{lb(a,Mb);}if(a->Xb!=ce){a->xc=1;a->rb=0;}if(a->rb!=0x00){int yc=(a->rb==Me)?1:0;if(Cf(a,a->kc,yc)!=0){a->j.I=a->of;}else{a->rb=0;}}a->Gd+=26;return a->Xb;}q _cpu;struct __init{__init(){ch=new sb[qc];Ug=new fe[qc];ge=new fe*[qc*2];q*a=&_cpu;a->df=Ai;a->ef=Bi;a->oc=new std::map<i,e>;}}___init;i eval_function(void*ui){q*a=&_cpu;ij(a);std::map<i,e>&map=*(std::map<i,e>*)a->oc;map.clear();a->j.I=reinterpret_cast<i>(ui);if(a->j.I!=_e)Ji(a->j.I);a->j.lc=reinterpret_cast<i>(gh+vf-8);*(i*)(gh+vf-8)=0;while(a->j.I){int Ii=aj(a);if(Ii==Og){printf("[*] CPU Exception (%d): [%d] %s at RIP 0x%016lx.\n",a->Xb,a->V.Hb,Zi(a->V.Hb),a->V.I);exit(1);}}return a->j.H;}}}