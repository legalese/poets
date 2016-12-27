%include polycode.fmt
%format :+: = "\mathbin{:\!\!+\!\!:}"
%format :&: = "\mathbin{:\!\!\&\!\!:}"
%format . = "\mathbin{.}"
%format forall = "\forall"
%format .. = "\cdots"
%format alpha = "\alpha"
%format tau = "\tau"
%format tau1 = "\tau_1"
%format tau2 = "\tau_2"
%format t_1
%format t_n
%format t_i
%format s_1
%format s_n
%format s_i
%format x_1
%format x_n
%format x_i
%format e1
%format e2
%format e3
%format f1
%format f2
%format f3
%format g1
%format g2
%format g3

In this section we briefly discuss some of the implementation
techniques used in our implementation of POETS. POETS is implemented
in Haskell~\cite{marlow10haskell}, and the logical structure of the
implementation reflects the diagram in Figure~\ref{fig:arch}, that is
each component is implemented as a separate Haskell module.

\subsection{External Interface}
The external interface to the POETS system is implemented in a
separate Haskell module. We currently use Thrift~\cite{slee07tr}
for implementing the communication layer between the server and its
clients, but other communication layers can in principle be
used. Changing the communication layer will only require a
change in one module.

Besides offering an abstract, light-weight interface to 
communication, Thrift enables type-safe communication. The types and
services of the server are specified in a language-independent
description language, from which Haskell code is generated (or code in
other languages for the clients). For example, the external interface
to querying a report can be specified as follows:
\begin{verbatim}
Value queryReport(
  1 : string name      // name of the report to execute
  2 : list<Value> args // input arguments
) throws (
  1 : ReportNotFoundException notFound
  2 : RunTimeException runtime
  3 : TypeException type
)
\end{verbatim}
From this specification, Thrift generates the Haskell code for the
server interface, and implementing the interface amounts to supplying
a function of the type |String -> [Value] -> IO Value|---namely the
query function.

\subsection{Domain-Specific Languages}
The main ingredient of the POETS implementation is the implementation
of the domain-specific languages. What is interesting  in that
respect---compared to implementations of domain-specific languages in
isolation of each other---is the common core shared by the languages,
in particular types and values.

In order to reuse and extend the structure of types and values in the
report language and the contract language, we make use of the
\emph{compositional data types}~\cite{bahr11wgp}
library. Compositional data types take the \emph{data types as
  fixed points}~\cite{meijer91fpca} view on abstract syntax trees
(ASTs), namely a separation of the recursive structure of ASTs from
their signatures. As an example, we define the signatures of types
from Section~\ref{sec:types} as follows:
\begin{code}
type RecordName      = String
data TypeConstant a  = TBool  | TInt  | ..
data TypeRecord a    = TRecord RecordName
data TypeList a      = TList a
data TypeEnt a       = TEnt RecordName
\end{code}

The signature for the types of the data model is then obtained by
combining the individual signatures above
|TSig = TypeConstant :+: TypeRecord :+: TypeList :+: TypeEnt|, where
|(:+:) :: (* -> *) -> (* -> *) -> * -> *| is the sum of two
functors. Finally, the data type for ASTs of types can be defined by
tying the recursive knot |T = Term TSig|, where |Term :: (* -> *) ->
*| is the functor fixed point.

Recursive functions over ASTs are defined as type classes, with one
instance per atomic signature. For instance, a pretty printer for
types can be defined as  follows:
\begin{code}
class Functor f => Render f where
  render :: f String -> String

instance Render TypeConstant where
  render TInt   = "Int"
  render TBool  = "Bool"
  ..

instance Render TypeRecord where
  render (TRecord r) = r

instance Render TypeList where
  render (TList tau) ="[" ++ tau ++ "]"

instance Render TypeEnt where
  render (TEnt r) = "<" ++ r ++ ">"
\end{code}
and pretty printing of terms is subsequently obtained by lifting the
|render| algebra to a catamorphism, that is a function of type |Render
f => Term f -> String|.

\paragraph{Extendability}
The first benefit of the approach above is that we can extend the
signature for types to fit, for example, the contract language as in
Figure~\ref{fig:contract-language-grammar}:
\begin{code}
type TypeVar         =  String
data TypeUnit a      =  TUnit
data TypeVar a       =  TVar TypeVar
data TypeFunction a  =  TFunction a a
\end{code}
Extending the pretty printer amounts to only providing the new cases:
\begin{code}
instance render TypeUnit where
  render TUnit = "()"
instance render TypeVar where
  render (TVar alpha) = alpha
instance render TypeFunction where
  render (TFunction tau1 tau2) = tau1 ++ " -> " ++ tau2
\end{code}

A similar modular encoding is used for the language of values:
\begin{code}
data Value a  =  VInt Int | VBool Bool | VString String | ..
\end{code}
and the signature of expressions in the contract language of
Figure~\ref{fig:contract-language-grammar} can be obtained by
providing the extensions compared to the language of values:
\begin{code}
type Var = String
data Exp a  =  EVar Var  |  ELambda Var a  |  EApply a a | ..
\end{code}
That is, |Term (Exp :+: Value)| represents the type of ASTs for
expressions of the contract language. Reusing the signature for (core)
values means that the values of Section~\ref{sec:values}, which are
provided as input to the system for instance in the
\emph{registerTransaction} function, can be automatically coerced to
the richer language of expressions. That is, values of type |Term
Value| can be readily used as values of type |Term (Exp :+: Value)|,
without explicit copying or translation.

Notice the difference in the granularity of (core) value signatures
and (core) type signatures: types are divided into three signatures,
whereas values are in one signature. The rule of thumb we apply is to
divide signatures only when a function needs the granularity. For
instance, the type inference algorithm used in the report language and
the contract language implements a simplification
procedure~\cite{fuh90tcs}, which reduces type constraints to
\emph{atomic} type constraints. In order to guarantee this
transformation invariant statically, we hence need a signature of
atomic types, namely |TypeConstant :+: TypeVar|, which prompts the
finer granularity on types.

\paragraph{Syntactic sugar}
Besides enabling a common core of ASTs and functions on them,
compositional data type enable AST transformations where the invariant
of the transformation is witnessed by the type. Most notably,
desugaring can be implemented by providing a signature for syntactic
sugar:
\begin{code}
data ExpSug a = ELet Var a a | ..
\end{code}
as well as a transformation to the core signature:
\begin{code}
instance Desugar ExpSug where
  desugar (ELet x e1 e2) = ELam x e2 `EApp` e1
  ..
\end{code}
This approach yields a desugaring function of the type |Term (ExpSug
:+: Exp :+: Value) -> Term (Exp :+: Value)|, which witnesses that the
syntactic sugar has indeed been removed.

Moreover, since we define the desugaring translation in the style of a
\emph{term homomorphism}~\cite{bahr11wgp}, we automatically get a
lifted desugaring function that propagates AST annotations, such as
source code positions, to the desugared term. This means, for
instance, that type error messages can provide detailed source
position information also for terms that originate from syntactic
sugar.

%%% Local Variables: 
%%% mode: haskell-latex
%%% TeX-master: tr
%%% End: 
