import json,os,urllib.request as U
Y=os.environ["MAKEFUN_ENV_URL"]
def J(p,b):
 return json.load(U.urlopen(U.Request(Y+p,data=json.dumps(b).encode())))
V=(0,100,320,330,500,900,0)
M=10**7
RP=[ord(c)-40 for c in"(*(-4:Z("]
KN=[(df,dr)for df in(-2,-1,1,2)for dr in(-2,-1,1,2)if abs(df*dr)==2]
D8=((1,0),(-1,0),(0,1),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1))
DR=[(),(),KN,D8[4:],D8[:4],D8,D8]
CM=[15]*64
CM[0]=13;CM[7]=14;CM[4]=12;CM[56]=7;CM[63]=11;CM[60]=3
B=[0]*64
S=1;C=0;EP=-1;KP=[4,60]
TT={};ND=0;NB=80000;TM=0;UN=[];X=TimeoutError
def pf(f):
 global B,S,C,EP
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
 EP=-1 if w[3]=="-" else ord(w[3][0])-97+(ord(w[3][1])-49)*8
def att(x,by):
 f=x&7
 for dy in(7,9):
  y=x-dy*by
  if 0<=y<64 and B[y]==by and abs((y&7)-f)==1:return 1
 for df,dr in KN:
  y=x+df+8*dr
  if 0<=y<64 and abs((y&7)-f)==abs(df)and B[y]==2*by:return 1
 for df,dr in D8:
  nf,nr=f+df,(x>>3)+dr;d=0
  while 0<=nf<8 and 0<=nr<8:
   p=B[nr*8+nf];d+=1
   if p:
    if p*by>0:
     t=abs(p)
     if t==5 or(t==6 and d==1)or(t==3 and df*dr)or(t==4 and not df*dr):return 1
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
    if 0<=f+df<8 and(B[j]*S<0 or j==EP):
     for z in pz:ap(i|j<<6|z<<12)
  else:
   for df,dr in DR[t]:
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
 global S,C,EP
 UN.append((B[:],S,C,EP,KP[:]))
 fr=m&63;to=(m>>6)&63;pr=m>>12
 p=B[fr]
 if abs(p)==1 and to==EP and fr&7!=to&7:B[to-8*S]=0
 B[to]=pr*S if pr else p;B[fr]=0
 if abs(p)==6:
  KP[S<0]=to
  if abs(to-fr)==2:
   rf,rt=(to+1,to-1)if to in(6,62)else(to-2,to+1);B[rt]=B[rf];B[rf]=0
 C&=CM[fr]&CM[to]
 EP=fr+8*S if abs(p)==1 and abs(to-fr)==16 else -1
 S=-S
def un(m):
 global S,C,EP
 b,S,C,EP,k=UN.pop()
 B[:]=b;KP[:]=k
def ev():
 s=0
 for i,p in enumerate(B):
  if p:
   t=abs(p);q=i if p>0 else i^56
   c=min(i&7,7-(i&7))+min(i>>3,7-(i>>3))
   w=(0,RP[q>>3],-45+11*c,0,0,0,20-5*c-5*(q>>3))[t]
   s+=V[t]+w if p>0 else -V[t]-w
 return s
def sc(m):
 v=B[(m>>6)&63]
 return 400000 if m==TM else 300000+V[abs(v)]*16-V[abs(B[m&63])]//16 if v else 0
def sr(d,a,b,ply):
 global ND,TM;ND+=1
 if ND>NB:raise X
 if d<=0:
  st=ev()*S
  if st>=b:return st
  if st>a:a=st
  TM=0;bt=a;ms=gen(1)
 else:
  k=(tuple(B),S);TM=TT.get(k,0);bt=-M;ms=gen(0)
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
  if a>=b:break
 if d>0:
  if not n:return -100000+ply if att(KP[S<0],-S) else 0
  TT[k]=bmv
 return bt
def bm():
 global ND,TM
 ND=0;ms=[]
 for m in gen(0):
  mk(m)
  if not att(KP[S>0],S):ms.append(m)
  un(m)
 best=ms[0]
 for d in range(1,60):
  try:
   a=-M;cb=best;TM=0
   ms.sort(key=lambda m:(m!=best,-sc(m)))
   for m in ms:
    mk(m);v=-sr(d-1,-M,-a,1);un(m)
    if v>a:a=v;cb=m
   best=cb
  except X:break
 return best
def uci(m):
 a=m&63;b=(m>>6)&63
 s=chr(97+(a&7))+str((a>>3)+1)+chr(97+(b&7))+str((b>>3)+1)
 if m>>12:s+=" nbrq"[(m>>12)-1]
 return s
if __name__=="__main__":
 while 1:
  g=J("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":
   TT.clear();pf(g["fen"])
   g=J("/move",{"game":g["game"],"move":uci(bm())})
