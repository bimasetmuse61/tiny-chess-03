import json,os,urllib.request as U
def J(p,b):
 return json.load(U.urlopen(U.Request(os.environ["MAKEFUN_ENV_URL"]+p,data=json.dumps(b).encode())))
V=(0,100,320,330,500,900,0)
KN=[(df,dr)for df in(-2,-1,1,2)for dr in(-2,-1,1,2)if abs(df*dr)==2]
D8=((1,0),(-1,0),(0,1),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1))
DR=[KN,D8[4:],D8[:4],D8,D8]
CM=[15-{0:2,7:1,4:3,56:8,63:4,60:12}.get(i,0)for i in range(64)]
B=[0]*64
S=1;C=0;KP=[0,0]
TT={};TM=0;UN=[];HH={};X=TimeoutError
def pf(f):
 global B,S,C
 B=[0]*64;w=f.split();i=0
 for ch in w[0]:
  if ch.isdigit():i+=int(ch)
  elif ch!="/":
   p="pnbrqk".index(ch.lower())+1
   if ch.islower():p=-p
   x=56-(i&56)+(i&7);B[x]=p
   if abs(p)==6:KP[p<0]=x
   i+=1
 S=1-2*(w[1]<"w")
 C=sum(1<<i for i,c in enumerate("KQkq")if c in w[2])
def att(x,by):
 f=x&7
 for dy in(7,9):
  y=x-dy*by
  if 0<=y<64 and B[y]==by and abs((y&7)-f)==1:return 1
 for df,dr in KN:
  y=x+df+8*dr
  if 0<=y<64 and abs((y&7)-f)==abs(df)and B[y]==2*by:return 1
 for df,dr in D8:
  nf,nr=f+df,x//8+dr;d=0
  while 0<=nf<8 and 0<=nr<8:
   p=B[nr*8+nf];d+=1
   if p:
    if p*by>0:
     t=abs(p)
     if(t>4 and(t==5 or d==1))or(t==3 and df*dr)or(t==4 and not df*dr):return 1
    break
   nf+=df;nr+=dr
 return 0
def gen(cap):
 ms=[];ap=ms.append;w6=6 if S>0 else 1
 for i,p in enumerate(B):
  if p*S<=0:continue
  t=abs(p);f=i&7;r=i>>3
  if t==1:
   d=8*S;o=i+d;pr=r==w6;pz=(5,4,3,2)if pr else(0,)
   if B[o]==0 and(not cap or pr):
    for z in pz:ap(i|o<<6|z<<12)
    if not cap and not pr and r==7-w6 and B[i+2*d]==0:ap(i|(i+2*d)<<6)
   for df in(-1,1):
    j=o+df
    if 0<=f+df<8 and B[j]*S<0:
     for z in pz:ap(i|j<<6|z<<12)
  else:
   for df,dr in DR[t-2]:
    nf,nr=f+df,r+dr
    while 0<=nf<8 and 0<=nr<8:
     j=nr*8+nf;q=B[j]
     if q*S<0 or not(q or cap):ap(i|j<<6)
     if q:break
     if t==2 or t==6:break
     nf+=df;nr+=dr
 if not cap:
  for bit,kf,kt in((1,4,6),(2,4,2),(4,60,62),(8,60,58)):
   md=(kf+kt)//2
   if C&bit and all(B[x]==0 for x in((md,kt)if kt>kf else(md,kt,kf-3)))and not(att(kf,-S)or att(md,-S)or att(kt,-S)):ap(kf|kt<<6)
 return ms
def mk(m):
 global S,C
 UN.append((B[:],S,C,KP[:]))
 fr=m&63;to=(m>>6)&63;pr=m>>12
 p=B[fr]
 B[to]=pr*S if pr else p;B[fr]=0
 if abs(p)==6:
  KP[S<0]=to
  if abs(to-fr)==2:
   rf,rt=(to+1,to-1)if to in(6,62)else(to-2,to+1);B[rt]=B[rf];B[rf]=0
 C&=CM[fr]&CM[to]
 S=-S
def un(m):
 global S,C
 b,S,C,k=UN.pop()
 B[:]=b;KP[:]=k
def ev():
 s=0
 for i,p in enumerate(B):
  if p:
   t=abs(p);q=i if p>0 else i^56
   c=min(i&7,7-(i&7))+min(i>>3,7-(i>>3))
   w=(0,(0,2,0,5,12,18,50,0)[q>>3],-45+11*c,0,0,0,0)[t]
   s+=(V[t]+w)*(p>0 or-1)
 return s
def sc(m):v=B[(m>>6)&63];return 400000 if m==TM else 300000+abs(v)*16-abs(B[m&63]) if v else HH.get(m,0)
def sr(d,a,b,ply):
 global ND,TM;ND+=1
 if ND>9e4:raise X
 if d<=0:
  if att(KP[S<0],-S):bt=-1e7;ms=gen(0)
  else:
   st=ev()*S
   if st>=b:return st
   if st>a:a=st
   bt=a;ms=gen(1)
 else:
  k=(tuple(B),S);e=TT.get(k)
  if e and e[0]>=d:
   v=e[1]
   if e[2]==0:return v
   if e[2]==1:a=max(a,v)
   else:b=min(b,v)
   if a>=b:return v
  TM=e[3] if e else 0;bt=-1e7;ms=gen(0)
 ms.sort(key=sc,reverse=True)
 n=0
 for m in ms:
  mk(m)
  if att(KP[S>0],S):un(m);continue
  n+=1
  v=-sr(d-1,-b,-a,ply+1)
  un(m)
  if v>bt:bt=v;bmv=m
  if v>a:a=v
  if a>=b:
   if not B[(m>>6)&63]:HH[m]=HH.get(m,0)+d
   break
 if not n:return ply-100000 if att(KP[S<0],-S) else 0 if d>0 else bt
 if d>0:TT[k]=(d,bt,1 if bt>=b else 2 if bt<=a else 0,bmv)
 return bt
def bm():
 global ND,TM
 ND=0;ms=[]
 for m in gen(0):
  mk(m)
  if not att(KP[S>0],S):ms.append(m)
  un(m)
 best=ms[0]
 for d in range(60):
  try:
   a=-1e7;TM=0
   ms.sort(key=lambda m:(m!=best,-sc(m)))
   for m in ms:
    mk(m);v=-sr(d,-1e7,-a,1);un(m)
    if v>a:a=v;cb=m
   best=cb
  except X:break
 return best
def uci(m):
 s=f"{chr(97+(m&7))}{(m>>3&7)+1}{chr(97+(m>>6&7))}{(m>>9&7)+1}"
 if m>>12:s+=" nbrq"[(m>>12)-1]
 return s
if __name__=="__main__":
 while 1:
  g=J("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":
   TT.clear();pf(g["fen"])
   g=J("/move",{"game":g["game"],"move":uci(bm())})
