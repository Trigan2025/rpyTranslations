#!/usr/bin/env python3
import sys, os, os.path, re, gettext
from unicodedata import normalize as NZ
from textwrap import dedent
__all__=['populate','fixEmpty','check','reorder','diff']

def getLocale():
   if os.name == 'nt':
      from locale import windows_locale
      from ctypes import windll
      l = windows_locale.get(windll.kernel32.GetThreadUILanguage(),None)
      if not l is None: return l
   else:
      for L in ['LC_ALL','LANG','LANGUAGE']:
         l = os.getenv(L)
         if not (l is None or l in ['C','POSIX']): return l.split('@')[0].split('.')[0]
   return 'en'
TR = None
LANG = getLocale()
LANG=(LANG,LANG.split('_')[0])
if os.path.isdir('./locale'):
	TR = gettext.translation('rpyTranslations', localedir='./locale', languages=LANG, fallback=True)
if (TR is None or type(TR) is gettext.NullTranslations) and os.path.isdir('../locale'):
	TR = gettext.translation('rpyTranslations', localedir='../locale', languages=LANG, fallback=True)
if TR is None or type(TR) is gettext.NullTranslations:
	TR = gettext.translation('rpyTranslations', languages=LANG, fallback=True)
if not TR is None:
	_ = lambda s,n=None: TR.gettext(s) if n is None else TR.ngettext(s,s,n)
else: _ = lambda s,n=None: s

class StrPipe:
	def __init__(self):
		self._s = ""
	def write(self, v):
		self._s += v
	def __call__(self):
		s, self._s = self._s, ""
		return s
SP = StrPipe()
def Throw(Type, *args, code=1, **kwargs):
	print(*args, file=SP, **kwargs)
	error = Type(SP(), code)
	raise error
def Error(*args, code=1, file=None, **kwargs):
	if not file is None: print(*args, file=file, **kwargs)
	else: print(*args, file=sys.stderr, **kwargs)
	exit(code)

def Line(FL, pos):
	xpos, L = FL[0], 0
	while pos > xpos:
		L += 1
		xpos += FL[L]
	return L+1

# Old strings should always be double-quote strings such as extracted by Ren'Py, new strings can be otherwise.
reDQ = r'"(\\"|[^"])*"'
reSQ = r"'(\\'|[^'])*'"
reTDQ = r'"""(\\"|""?[^"]|[^"])*"""'
reTSQ = r"'''(\\'|''?[^']|[^'])*'''"
reQ = r'('+reTDQ+r'|'+reTSQ+r'|'+reDQ+r'|'+reSQ+r')'
reP = r'(?P<p>_p\()?{Q}(?(p)\))'.format(Q=reQ)
reN_old = r'(?P<dID>(?P<old_NID>{SQ}|{DQ}|[a-zA-Z_]+[a-zA-Z_0-9]*)(?P<idA>( +[a-zA-Z_]+[a-zA-Z_0-9]*)*( +@( +[a-zA-Z_]+[a-zA-Z_0-9]*)+)?))'.format(SQ=reSQ,DQ=reDQ)
reN_new = r'(?P<new_dID>(?P<new_NID>{SQ}|{DQ}|[a-zA-Z_]+[a-zA-Z_0-9]*)(?P<new_idA>( +[a-zA-Z_]+[a-zA-Z_0-9]*)*( +@( +[a-zA-Z_]+[a-zA-Z_0-9]*)+)?))'.format(SQ=reSQ,DQ=reDQ)
rePy = r'\$(\\\r?\n|[^\n])*'# does not support logical line for '(', '{' and '[' and reQ
rePass = r'pass( *#[^\n]*|\r?\n)'
reRID = r'# (?P<file>[^;:\\/]+(/[^;:\\/]+)*\.rpy):(?P<line>0|[1-9][0-9]*)'

reFrm=r'({{(?P<dbF>(\\}}|}?[^}])*)}}|{(?P<bF>(\\}|[^}])*)}|\[(?P<bkF>(\\\]|[^]])*)\])'

reString = r'^(([ \t]+##[^\n]*\r?\n)*([ \t]+{rID}\r?\n)?([ \t]+(?P<old>{old})([ \t]+#[^\n]*)?(\r?\n([ \t]+##[^\n]*)?)*))\r?\n[ \t]+(?P<new>{new})([ \t]+#[^\n]*)?(\r?\n[ \t]+##?[^\n]*)*\r?\n(\r?\n)?'
reStringCmt = r'^(([ \t]+##[^\n]*\r?\n)*([ \t]+{rID}\r?\n)?([ \t]+#(?P<old>{old})([ \t]+#[^\n]*)?(\r?\n([ \t]+##[^\n]*)?)*))\r?\n[ \t]+#(?P<new>{new})([ \t]+#[^\n]*)?(\r?\n[ \t]+##?[^\n]*)*\r?\n(\r?\n)?'# This is mainly to use with the 'diff' command
reDialog = r'^((##[^\n]*\r?\n)*({rID}\r?\n)?(translate +[a-zA-Z]+[a-zA-Z_]* +(?P<TID>[a-zA-Z_]+[a-zA-Z_0-9]*) *:([ \t]+#[^\n]*)?\r?\n((\s*##[^\n]*)*\s*)*(?P<prePy>(\r?\n([ \t]+##[^\n]*|[ \t]+{py})*|[ \t]*)*)[ \t]+(?P<old>{old})(?P<old_pArgs>[ \t]*[a-zA-Z_]+[a-zA-Z_0-9]*(\([^)]*\))?( [a-zA-Z_]+[a-zA-Z_0-9]*(\([^)]*\))?)*)?([ \t]+#[^\n]*)?(?P<Py>(\r?\n([ \t]+##[^\n]*|[ \t]+{py})*|[ \t]*)*)))\r?\n[ \t]+(?P<new>{new})(?P<new_pArgs>[ \t]*[a-zA-Z_]+[a-zA-Z_0-9]*(\([^)]*\))?( [a-zA-Z_]+[a-zA-Z_0-9]*(\([^)]*\))?)*)?([ \t]*#[^\n]*)?(\r?\n([ \t]+##?[^\n]*|[ \t]*))*\r?\n(\r?\n)?'
# The final `(\r?\n)?` is to ease and enance the work of 'fixEmpty' and 'reorder' functions.

RE_S = re.compile(r'\s+')# for splittings
RE_Py = re.compile(rePy)
RE_pass = re.compile(rePass)
RE_refid = re.compile(reRID)
def _p(s):# fake function for the Ren'Py `_p` translation function
	_s = dedent(s)
	_s = re.sub(r'([^\n])(?<!{p})\r?\n([^\r\n])', r'\1 \2', _s, re.M|re.S)
	_s = re.sub(r'({p})+', r'{p}', _s, re.M|re.S)
	_s = re.sub(r'(\r?\n){p}|{p}(\r?\n)', r'\1\2', _s, re.M|re.S)
	return _s.replace('{p}', '\n').strip()

def N_str(s):
	S, res = [x.encode() for x in RE_S.split(NZ('NFKD',s))], ""
	for ch in S:
		res += ''.join([chr(c) for c in ch if chr(c).isascii() and chr(c).isalnum()])
	return res

def cmp(str1, str2):
	score = count = i = 0
	s1,s2 = [x.encode() for x in RE_S.split(NZ('NFKD',str1))],[x.encode() for x in RE_S.split(NZ('NFKD',str2))]
	s1 = ''.join([chr(c) for c in b''.join(s1) if chr(c).isascii() and chr(c).isalnum()])
	s2 = ''.join([chr(c) for c in b''.join(s2) if chr(c).isascii() and chr(c).isalnum()])
	count = len(s1)
	s = len(s1)-len(s2)
	if s < 0: s1 += ' '*(-s)
	for c1 in s1:
		for c2 in s2[i:]:
			i += 1
			if c1 == c2:
				score += 1
				break
			else:
				score -= 1
	return score/count if count != 0 else 0
cmp.__doc__ = _("""This function try to estimate the proxymity between two strings.
Please note that this can hardly be precise and error-free.
""")

def diff(forFiles, *FromFiles, newpart=True, what=False, reflines=False, trID=False, verbose=0, debug=False):
	Verb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=1 else None
	xVerb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=2 else None
	Debug = lambda *args, **kwargs: print(*args, **kwargs) if debug else None
	if not isinstance(newpart, bool):
		Throw(TypeError,_("Invalide argument:"),_("{} parameter should be of booleen type").format('newpart'), code=2)
	if not isinstance(what, bool):
		Throw(TypeError,_("Invalide argument:"),_("{} parameter should be of booleen type").format('what'), code=2)
	if not isinstance(reflines, bool):
		Throw(TypeError,_("Invalide argument:"),_("{} parameter should be of booleen type").format('reflines'), code=2)
	if not isinstance(trID, bool):
		Throw(TypeError,_("Invalide argument:"),_("{} parameter should be of booleen type").format('trID'), code=2)
	for fromFiles in FromFiles:
		if len(forFiles) != len(fromFiles):
			Throw(Exception,_("The length of both list of files need to be equal"))
		for forF, fromF in zip(forFiles, fromFiles):
			if not os.path.isfile(forF):
				Throw(FileNotFoundError,repr(forF),_("doen’t exist or is not a file"))
			if not os.path.isfile(fromF):
				Throw(FileNotFoundError,repr(fromF),_("doen’t exist or is not a file"))
			if forF == fromF:
				Throw(Exception,_("Both list of files need to have their files different"))

	RE_string = re.compile(reString.format(old=r'old +(?P<old_str>{Q})'.format(Q=reDQ), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
	RE_stringCmt = re.compile(reStringCmt.format(old=r'old +(?P<old_str>{Q})'.format(Q=reDQ), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
	RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{Q})'.format(N=reN_old, Q=reDQ), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN_new, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
	file_cache, frm_res = {}, {}
	for fromFiles in FromFiles:# file caching loop
		for forF, fromF in zip(forFiles, fromFiles):
			Debug('forFile:',repr(forF))
			if not forF in file_cache.keys():
				F = ""
				with open(forF, 'r', encoding='utf-8', newline='') as f:
					L, file_cache[forF+':L'] = 0, []
					for line in f:
						L += len(line)
						file_cache[forF+':L'].append(len(line))
						F += line
					Debug('Debug: total size:',L,'; lines:',len(file_cache[forF+':L']))
				file_cache[forF] = F
			Debug('fromFile:',repr(fromF))
			if not fromF in file_cache.keys():
				F = ""
				with open(fromF, 'r', encoding='utf-8', newline='') as f:
					L, file_cache[fromF+':L'] = 0, []
					for line in f:
						L += len(line)
						file_cache[fromF+':L'].append(len(line))
						F += line
					Debug('Debug: total size:',L,'; lines:',len(file_cache[forF+':L']))
				file_cache[fromF] = F

	def get_M(M1, M2, M3):
		M, T = None, 0
		if not M1 is None and not M3 is None and not M2 is None:
			if M2.start() < M1.start():
				if M2.start() < M3.start():
					M, T = M2, 1
				else:
					M, T = M3, 2
			elif M1.start() < M3.start():
				M, T = M1, 0
			else:
				M, T = M_stringCmt, 2
		elif not M2 is None:
			if not M3 is None and M3.start() < M2.start():
				M, T = M_stringCmt, 2
			else:
				M, T = M2, 1
		elif not M1 is None:
			if not M3 is None and M3.start() < M1.start():
				M, T = M3, 2
			else:
				M, T = M1, 0
		else:
			M, T = M3, 2
		return M, T
	for fromFiles in FromFiles:# process loop
		for forF, fromF in zip(forFiles, fromFiles):
			Verb('forFile:',repr(forF))
			Verb('fromFile:',repr(fromF))
			_for_spans, _from_spans, res = [], [], ([],[])
			# checking for removed ones and the general differences
			pos, TID = 0, None
			M, M_from, T, M_string, M_stringCmt, M_dialog = None, None, 0, RE_string.search(file_cache[fromF], pos), RE_stringCmt.search(file_cache[fromF], pos), RE_dialog.search(file_cache[fromF], pos)
			while not (M_string is None and M_stringCmt is None and M_dialog is None):
				M, T = get_M(M_string, M_dialog, M_stringCmt)
				M_from, T_from, Pass = None, 0, not RE_pass.match(M.group('new')) is None
				Debug(f'Debug:match span {M.span()} is:\n{M.group()!r}')
				Debug("Debug: new_str group =",repr(M.group('new_str')) if not Pass else 'pass')
				if T == 1:
					_RE_dialog = re.compile(reDialog.format(old=re.escape(M.group('old')), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN_new, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
					M_from = m_from = _RE_dialog.search(file_cache[forF])
					T_from, TID = 1, M.group('TID')
					while not m_from is None and (m_from.group('TID') != TID or m_from.span() in _for_spans):
						m_from = _RE_dialog.search(file_cache[forF], m_from.end())
						if not m_from is None and m_from.group('TID') == TID:
							M_from = m_from
						elif not (m_from is None or m_from.span() in _for_spans):
							M_from = m_from
				else:
					_RE_string = re.compile(reString.format(old=re.escape(M.group('old')), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
					_RE_stringCmt = re.compile(reStringCmt.format(old=re.escape(M.group('old')), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
					M_from = m_from = _RE_string.search(file_cache[forF])
					T_from = 0
					while not m_from is None and m_from.span() in _for_spans:
						m_from = _RE_string.search(file_cache[forF], m_from.end())
						if not (m_from is None or m_from.span() in _for_spans):
							M_from = m_from
					if M_from is None:
						T_from = 3
						M_from = m_from = _RE_stringCmt.search(file_cache[forF])
						while not m_from is None and m_from.span() in _for_spans:
							m_from = _RE_stringCmt.search(file_cache[forF], m_from.end())
							if not (m_from is None or m_from.span() in _for_spans):
								M_from = m_from
				if M_from is None:
					xVerb("Is removed:",repr(M.group('old')))
					if what:
						if T == 1:
							refL = '' if M.group('file') is None else ':'.join((M.group('file'),M.group('line')))
							sO = M.group('old')+('' if M.group('old_pArgs') is None else M.group('old_pArgs'))
							sN = M.group('new')+('' if M.group('new_pArgs') is None else M.group('new_pArgs'))
							prePy, Py, sPy = M.group('prePy'), M.group('Py'), []
							mPy = RE_Py.search(prePy)
							while not mPy is None:
								sPy.append(mPy.group())
								mPy = RE_Py.search(prePy, mPy.end())
							mPy = RE_Py.search(Py)
							while not mPy is None:
								sPy.append(mPy.group())
								mPy = RE_Py.search(Py, mPy.end())
							S = (refL,sO,sN,M.group('TID'),sPy)
						else:
							refL = '' if M.group('file') is None else ':'.join((M.group('file'),M.group('line')))
							S = (refL,M.group('old_str'),M.group('new_str'))
						res[0].append((*M.span(),("string","dialog","commented-string")[T],S))
					else: res[0].append((*M.span(),""))
				else:
					if T_from == 1:# to ensure all groups are present
						m_from = M_from
						M_from = RE_dialog.search(file_cache[forF], M_from.start())
						if M_from is None: Debug("Debug: M is",repr(M.group()),"\n  M_from is",repr(m_from.group()))
						Debug("Debug: M is",repr(M),"\n  M_from is",repr(M_from))
					_for_spans.append(M_from.span())
					_res = []
					if reflines and not M.group('file') is None:
						if M_from.group('file') is None:
							rf = ':'.join((M.group('file'),M.group('line')))
							xVerb("The reference-line is removed:",rf)
							_res.append(("reference-line","removed",rf))
						elif M.group('file') != M_from.group('file'):
							rf = ':'.join((M.group('file'),M.group('line')))
							to_rf = ':'.join((M_from.group('file'),M_from.group('line')))
							xVerb("The file of the reference-line as changed from:",rf,"to:",to_rf)
							_res.append(("reference-line","file",rf,to_rf))
						elif M.group('line') != M_from.group('line'):
							rf = ':'.join((M.group('file'),M.group('line')))
							to_rf = ':'.join((M_from.group('file'),M_from.group('line')))
							xVerb("The line in the reference-line as changed from:",rf,"to:",to_rf)
							_res.append(("reference-line","line",rf,to_rf))
					if T == 1:
						if trID and TID != M_from.group('TID'):
							tid = M_from.group('TID')
							xVerb("The file of the translation-identifier as changed from:",TID,"to:",tid)
							_res.append(("translation-id",TID,tid))
						if M.group('old_NID') != M_from.group('old_NID'):
							nId,nid = M.group('old_NID'), M_from.group('old_NID')
							xVerb("The name of the dialog-identifier as changed from:",nId,"to:",nid)
							_res.append(("dialog-id","name",nId,nid))
						elif M.group('dID') != M_from.group('dID'):
							dId,did = M.group('dID'), M_from.group('dID')
							xVerb("The dialog-identifier as changed from:",dId,"to:",did)
							_res.append(("dialog-id","attr",dId,did))
						if not M.group('old_pArgs') is None:
							if M_from.group('old_pArgs') is None:
								xVerb("The translation had now post-string arguments:",M.group('old_pArgs'))
								_res.append(("post-args","lost",M.group('old_pArgs')))
							elif M.group('old_pArgs') != M_from.group('old_pArgs'):
								xVerb("The post-string arguments had changed from:",M.group('old_pArgs'),"to:",M_from.group('old_pArgs'))
								_res.append(("post-args","changed",M.group('old_pArgs'),M_from.group('old_pArgs')))
						else:
							if not M_from.group('old_pArgs') is None:
								xVerb("The translation have no longer post-string arguments:",M_from.group('old_pArgs'))
								_res.append(("post-args","gained",M_from.group('old_pArgs')))
					if newpart and T == 2:
						if T_from == 0:
							xVerb("As been uncommented:",repr(M.group('old')))
							_res.append(("commented",False))
					elif newpart and T == 0:
						if T_from == 2:
							xVerb("As been commented:",repr(M.group('old')))
							_res.append(("commented",True))
					if newpart and Pass:
						if RE_pass.match(M_from.group('new')) is None:
							xVerb("As been unset from pass:",repr(M.group('old')))
							_res.append(("pass",False))
					elif newpart and M.group('new') != M_from.group('new'):
						D = 0
						if not RE_pass.match(M_from.group('new')) is None:
							xVerb("As been set to pass:",repr(M.group('old')))
							_res.append(("pass",True))
							D += 1
						elif M.group('new_str') == '""':
							xVerb("Are get a translation:",repr(M.group('old')))
							_res.append(("empty",False,M_from.group('new')))
							D += 1
						elif M_from.group('new_str') == '""':
							xVerb("Are loose his translation:",repr(M.group('old')))
							_res.append(("empty",True,M.group('new')))
							D += 1
						else:
							o_str,n_str = eval(M.group('new_str')),eval(M_from.group('new_str'))# since we now they are string, np
							o,n = re.search(r'^\s*', o_str),re.search(r'^\s*', n_str)
							if o.group() != n.group():
								_res.append((o.span(),"Leading white-spaces","start",repr(o.group()),repr(n.group())))
								D += 1
							o,n = re.search(r'\s*$', o_str),re.search(r'\s*$', n_str)
							if o.group() != n.group():
								_res.append((o.span(),"Leading white-spaces","end",repr(o.group()),repr(n.group())))
								D += 1
							RE_x = re.compile(r'\s*{x}\s*'.format(x=reFrm))
							o,n, n_end = RE_x.search(o_str),RE_x.search(n_str), 0
							while not o is None:
								if n is None:
									_res.append((o.span(),"Formating",repr(o.group()),None))
									D += 1
								elif o.group() != n.group():
									n_end = n.end()
									_o,_n = re.search(reFrm, o.group()),re.search(reFrm, n.group())
									if _o.group() != _n.group():
										_res.append((o.span(),"Formating",repr(o.group()),repr(n.group())))
										D += 1
									else:
										_o,_n = re.search(r'^\s*', o.group()),re.search(r'^\s*', n.group())
										if _o.group() != _n.group():
											_res.append((o.span(),"Leading white-spaces",_o.span(),repr(o.group()),repr(n.group())))
											D += 1
										_o,_n = re.search(r'\s*$', o.group()),re.search(r'\s*$', n.group())
										if _o.group() != _n.group():
											_res.append((o.span(),"Leading white-spaces",_o.span(),repr(o.group()),repr(n.group())))
											D += 1
								else: n_end = n.end()
								o,n = RE_x.search(o_str, o.end()),RE_x.search(n_str, n_end)
						if D == 0 and M.group('new_str') != M_from.group('new_str'):# another kind of difference
							_res.append(("diff",M.group('new_str'),M_from.group('new_str')))
					if _res != []:
						res[0].append((*M.span(),_res))
				pos = M.end()
				M_string, M_stringCmt, M_dialog = RE_string.search(file_cache[fromF], pos), RE_stringCmt.search(file_cache[fromF], pos), RE_dialog.search(file_cache[fromF], pos)
			# checking for added ones
			pos = 0
			M, T, M_string, M_stringCmt, M_dialog = None, 0, RE_string.search(file_cache[forF], pos), RE_stringCmt.search(file_cache[forF], pos), RE_dialog.search(file_cache[forF], pos)
			while not (M_string is None and M_stringCmt is None and M_dialog is None):
				M, T = get_M(M_string, M_dialog, M_stringCmt)
				refL = ''
				if M.span() in _for_spans:
					pos = M.end()
					M_string, M_stringCmt, M_dialog = RE_string.search(file_cache[forF], pos), RE_stringCmt.search(file_cache[forF], pos), RE_dialog.search(file_cache[forF], pos)
					continue
				Pass = not RE_pass.match(M.group('new')) is None
				# _for_spans contain all matching span with fromFile, so if we’re here we already now it’s add
				if what:
					refL = '' if M.group('file') is None else ':'.join((M.group('file'),M.group('line')))
				if Pass:
					xVerb("Set to pass but added:",repr(M.group('old')))
					if what:
						if T == 1:
							sO = M.group('old')+('' if M.group('old_pArgs') is None else M.group('old_pArgs'))
							S = (refL,M.group('old'),M.group('TID'))
						else: S = (refL,M.group('old'))
						res[1].append((*M.span(),"passed","dialog",S))
					else: res[1].append((*M.span(),"passed"))
				elif T == 2:
					xVerb("Commented but added:",repr(M.group('old')))
					if what:
						if T == 1:
							sO = M.group('old')+('' if M.group('old_pArgs') is None else M.group('old_pArgs'))
							S = (refL,M.group('old'),M.group('TID'))
						else: S = (refL,M.group('old'))
						res[1].append((*M.span(),"commented","commented-string",S))
					else: res[1].append((*M.span(),"commented"))
				elif M.group('new_str') == '""':
					xVerb("Empty but added:",repr(M.group('old')))
					if what:
						if T == 1:
							sO = M.group('old')+('' if M.group('old_pArgs') is None else M.group('old_pArgs'))
							S = (refL,M.group('old'),M.group('TID'))
						else: S = (refL,M.group('old'))
						res[1].append((*M.span(),"empty",("string","dialog")[T],S))
					else: res[1].append((*M.span(),"empty"))
				else:
					xVerb("Added:",repr(M.group('old')))
					if what:
						if T == 1:
							sO = M.group('old')+('' if M.group('old_pArgs') is None else M.group('old_pArgs'))
							sN = M.group('new')+('' if M.group('new_pArgs') is None else M.group('new_pArgs'))
							prePy, Py, sPy = M.group('prePy'), M.group('Py'), []
							mPy = RE_Py.search(prePy)
							while not mPy is None:
								sPy.append(mPy.group())
								mPy = RE_Py.search(prePy, mPy.end())
							mPy = RE_Py.search(Py)
							while not mPy is None:
								sPy.append(mPy.group())
								mPy = RE_Py.search(Py, mPy.end())
							S = (refL,sO,sN,M.group('TID'),sPy)
						else:
							S = (refL,M.group('old_str'),M.group('new_str'))
						res[1].append((*M.span(),"added",("string","dialog","commented-string")[T],S))
					else: res[1].append((*M.span(),"added"))
				pos = M.end()
				M_string, M_stringCmt, M_dialog = RE_string.search(file_cache[forF], pos), RE_stringCmt.search(file_cache[forF], pos), RE_dialog.search(file_cache[forF], pos)
			if res != ([],[]):
				if not forF in frm_res.keys(): frm_res[forF] = {}
				frm_res[forF][fromF] = res

	for forF, frmF in frm_res.items():
		S, fromFiles = f"For: {forF}", sorted(frmF.keys())
		for fromF in fromFiles:
			S += f"\nFrom: {fromF}"
			if len(frmF[fromF][1]) > 0:
				for k in frmF[fromF][1]:
					at = [str(Line(file_cache[forF+':L'], n)) for n in k[:2]]
					S += f"\n+@Lines {':'.join(at)}: ADDED"
					if k[2] == "added" and what:
						W = k[4]
						S += "\n: "+k[3]
						if W[0] != '': S += " :: "+W[0]
						if k[3] == "dialog":
							S += " :: "+W[3]
							for Py in W[4]: S += "\n>\t"+repr(Py)
						S += f"\n>\t{repr(W[1])}\n>\t{repr(W[2])}"
					elif k[2] == "passed":
						S += " but set with 'pass'"
						if what:
							W = k[4]
							S += "\n: "+k[3]
							if W[0] != '': S += " :: "+W[0]
							if len(W) > 2:
								S += " :: "+W[2]
							S += f"\n>\t{repr(W[1])}"
					else:
						S += f" but {k[2]}"
						if what:
							W = k[4]
							S += "\n: "+k[3]
							if W[0] != '': S += " :: "+W[0]
							if len(W) > 2:
								S += " :: "+W[2]
							S += f"\n>\t{repr(W[1])}"
			if len(frmF[fromF][0]) > 0:
				for k in frmF[fromF][0]:
					at = [str(Line(file_cache[fromF+':L'], n)) for n in k[:2]]
					if isinstance(k[2], str):
						S += f"\n-@Lines {':'.join(at)}: REMOVED"
						if what:
							W = k[3]
							S += "\n: "+k[2]
							if W[0] != '': S += " :: "+W[0]
							if k[2] == "dialog":
								S += " :: "+W[3]
								for Py in W[4]: S += "\n<\t"+repr(Py)
							S += f"\n<\t{repr(W[1])}\n<\t{repr(W[2])}"
					else:
						for frm in k[2]:
							S += f"\n!@Lines {':'.join(at)}:"
							if frm[0] == "reference-line":
								if frm[1] == "file":
									S += f" Reference-line file changed\n<\t{frm[2]}\n>\t{frm[3]}"
								elif frm[1] == "line":
									S += f" Reference-line line changed\n<\t{frm[2]}\n>\t{frm[3]}"
								else:# frm[1] == "removed":
									S += f" Reference-line removed ({frm[2]})"
							elif frm[0] == "translation-id":
								S += f" Translation-identifier changed\n<\t{frm[1]}\n>\t{frm[2]}"
							elif frm[0] == "dialog-id":
								if frm[1] == "name":
									S += f" Dialog-identifier changed\n<\t{frm[2]}\n>\t{frm[3]}"
								else:# frm[1] == "attr":
									S += f" Dialog-identifier changed\n<\t{frm[2]}\n>\t{frm[3]}"
							elif frm[0] == "post-args":
								if frm[1] == "lost":
									S += f" Dialog post-string arguments: LOST\n<\t{frm[2]}"
								elif frm[1] == "gained":
									S += f" Dialog post-string arguments: GAINED\n>\t{frm[2]}"
								else:# frm[1] == "changed":
									S += f" Dialog post-string arguments: CHANGED\n<\t{frm[2]}\n>\t{frm[3]}"
							elif frm[0] == "commented":
								if frm[1]: S += " COMMENTED"
								else: S += " UNCOMMENTED"
							elif frm[0] == "pass":
								if frm[1]: S += " SET TO PASS"
								else: S += " UNSET TO PASS"
							elif frm[0] == "empty":
								if frm[1]: S += f" NEW: The following as been add from EMPTY\n\t"+repr(frm[2])
								else: S += " LOST: The following as been changed to EMPTY\n\t"+repr(frm[2])
							elif frm[0] == "diff":
								S += f"\n<\t{frm[1]!r}\n>\t{frm[2]!r}"
							elif frm[1] == "Leading white-spaces":
								at_pos = [str(n) for n in frm[0]]
								if frm[2] in ("start","end"):
									S += f"\n\t@ {':'.join(at_pos)}: Leading white-spaces at {frm[2]}:"
								else:
									into = [str(n) for n in frm[2]]
									S += f"\n\t@ {':'.join(at_pos)}: Leading white-spaces at {':'.join(into)}:"
								S += f"\n\t<\t{frm[3]}\n\t>\t{frm[4]}"
							else:# frm[1] == "Formating":
								at_pos = [str(n) for n in frm[0]]
								S += f"\n\t@ {':'.join(at_pos)}: Formating:"
								if not frm[3] is None:
									S += f"\n\t<\t{frm[2]}\n\t>\t{frm[3]}"
								else: S += f"\n\t<\t{frm[2]}\n\t-\tmissing in this translation"
		Verb(S)
		fPath = os.path.split(forF)
		fName = os.path.splitext(fPath[1])[0]+'.diff'
		with open(os.path.join(fPath[0],fName), 'w', encoding='utf-8') as f:
			f.write(S)
diff.__doc__ = _("""\
forFiles: List - The list of files where differences of translation need to be compared.
FromFiles: List,... - Successions of list of files from where to get other translations.
newpart: Booleen - Output or not comparisons of new-part of translations.
what: Booleen - Output or not of what is add or is no longer present in the translation files.
reflines: Booleen - Output or not comparisons of reference-lines.
trID: Booleen - Output or not comparisons of translation-identifiers.
verbose: Integer - Indicates the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
""")

def check(forFiles, /, untranslated=1, where=False, formats=False, *, verbose=0, debug=False):
	Verb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=1 else None
	xVerb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=2 else None
	Debug = lambda *args, **kwargs: print(*args, **kwargs) if debug else None
	if not untranslated in (0,1,2):
		Throw(ValueError,_("Invalide argument:"),_("{} parameter should be 1 for empty, 2 for pass, or 0 to deactivate").format('untranslated'), code=2)
	if not isinstance(where, bool):
		Throw(TypeError,_("Invalide argument:"),_("{} parameter should be of booleen type").format('where'), code=2)
	if not isinstance(formats, bool):
		Throw(TypeError,_("Invalide argument:"),_("{} parameter should be of booleen type").format('formats'), code=2)
	for i,forF in enumerate(forFiles):
		if not os.path.isfile(forF):
			Throw(FileNotFoundError,repr(forF),_("doen’t exist or is not a file"))
		if forF in forFiles[:i]: Throw(Exception,_("All files need to be different"))

	RE_string = re.compile(reString.format(old=r'old +(?P<old_str>{Q})'.format(Q=reDQ), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
	RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{Q})'.format(N=reN_old, Q=reDQ), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN_new, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
	file_cache, frm_res = {}, {}
	for forF in forFiles:
		Verb('forFile:',repr(forF))
		if not forF in file_cache.keys():
			F = ""
			with open(forF, 'r', encoding='utf-8', newline='') as f:
				L, file_cache[forF+':L'] = 0, []
				for line in f:
					L += len(line)
					file_cache[forF+':L'].append(len(line))
					F += line
				Debug('Debug: total size:',L,'; lines:',len(file_cache[forF+':L']))
			file_cache[forF] = F

		ppl = 0
		M, T, M_string, M_dialog = None, 0, RE_string.search(file_cache[forF], 0), RE_dialog.search(file_cache[forF], 0)
		while not (M_string is None and M_dialog is None):
			if not M_string is None and not M_dialog is None:
				if M_dialog.start() < M_string.start():
					M, T = M_dialog, 1
				else:
					M, T = M_string, 0
			elif M_dialog is None:
				M, T = M_string, 0
			else:
				M, T = M_dialog, 1
			Pass = not RE_pass.match(M.group('new')) is None
			Debug(f'Debug:match span {M.span()} is:\n{M.group()!r}')
			Debug("Debug: new_str group =",repr(M.group('new_str')) if not Pass else 'pass')
			if Pass or M.group('new_str') == '""':
				if M.group('old_str') == '""': pass
				elif not Pass and untranslated >= 1:
					ppl += 1
					xVerb(f"For: {M.group('old')}\nTranslation is: {M.group('new')}")
					if where:
						if not forF in frm_res.keys(): frm_res[forF] = {}
						frm_res[forF][M.span()] = ("empty",)
				elif untranslated == 2:
					ppl += 1
					xVerb(f"For: {M.group('old')}\nTranslation is: pass")
					if where:
						if not forF in frm_res.keys(): frm_res[forF] = {}
						frm_res[forF][M.span()] = ("pass",)
			else:
				res = []
				if T == 1:
					if not M.group('dID') is None:
						nId,nid = M.group('old_NID'), M.group('new_NID')
						if not re.match(r'^{SQ}|{DQ}$'.format(SQ=reSQ,DQ=reDQ), nId) is None:
							if nId == nid:
								xVerb("The new string name of the dialog-identifier as not changed from:",nId)
								res.append(("dialog-id","name-str",nId))
							elif M.group('dID') != M.group('new_dID'):# For now it will also match for the translated name difference, but it is ok
								dId,did = M.group('dID'), M.group('new_dID')
								xVerb("The new dialog-identifier as changed from:",dId,"to:",did)
								res.append(("dialog-id","attr",dId,did))
						else:
							if nId != nid:
								xVerb("The new name of the dialog-identifier as changed from:",nId,"to:",nid)
								res.append(("dialog-id","name",nId,nid))
							elif M.group('dID') != M.group('new_dID'):
								dId,did = M.group('dID'), M.group('new_dID')
								xVerb("The new dialog-identifier as changed from:",dId,"to:",did)
								res.append(("dialog-id","attr",dId,did))
					if not M.group('old_pArgs') is None:
						if M.group('new_pArgs') is None:
							xVerb("The translation lost its post-string arguments:",M.group('old_pArgs'))
							res.append(("post-args","lost",M.group('old_pArgs')))
						elif M.group('old_pArgs') != M.group('new_pArgs'):
							xVerb("The post-string arguments had changed from:",M.group('old_pArgs'),"to:",M.group('new_pArgs'))
							res.append(("post-args","changed",M.group('old_pArgs'),M.group('new_pArgs')))
					else:
						if not M.group('new_pArgs') is None:
							xVerb("The translation gained post-string arguments:",M.group('new_pArgs'))
							res.append(("post-args","gained",M.group('new_pArgs')))
				if formats:
					o_str,n_str = eval(M.group('old_str')),eval(M.group('new_str'))# since we now they are string, np
					o,n = re.search(r'^\s*', o_str),re.search(r'^\s*', n_str)
					if o.group() != n.group():
						res += [(o.span(),"Leading white-spaces","start",repr(o.group()),repr(n.group()))]
					o,n = re.search(r'\s*$', o_str),re.search(r'\s*$', n_str)
					if o.group() != n.group():
						res += [(o.span(),"Leading white-spaces","end",repr(o.group()),repr(n.group()))]
					RE_x = re.compile(r'\s*{x}\s*'.format(x=reFrm))
					o,n, n_end = RE_x.search(o_str),RE_x.search(n_str), 0
					while not o is None:
						if n is None:
							res += [(o.span(),"Formating",repr(o.group()),None)]
						elif o.group() != n.group():
							n_end = n.end()
							_o,_n = re.search(reFrm, o.group()),re.search(reFrm, n.group())
							if _o.group() != _n.group():
								res += [(o.span(),"Formating",repr(o.group()),repr(n.group()))]
							else:
								_o,_n = re.search(r'^\s*', o.group()),re.search(r'^\s*', n.group())
								if _o.group() != _n.group():
									res += [(o.span(),"Leading white-spaces",_o.span(),repr(o.group()),repr(n.group()))]
								_o,_n = re.search(r'\s*$', o.group()),re.search(r'\s*$', n.group())
								if _o.group() != _n.group():
									res += [(o.span(),"Leading white-spaces",_o.span(),repr(o.group()),repr(n.group()))]
						else: n_end = n.end()
						o,n = RE_x.search(o_str, o.end()),RE_x.search(n_str, n_end)
				if res != []:
					if not forF in frm_res.keys(): frm_res[forF] = {}
					frm_res[forF][M.span()] = res

			M_string, M_dialog = RE_string.search(file_cache[forF], M.end()), RE_dialog.search(file_cache[forF], M.end())
		file_cache[forF+':E'] = ppl

	RE_if, ifs = re.compile(r'^[ \t]+if ', re.M), 0
	for forF in forFiles:
		xVerb('forFile:',repr(forF))
		_ifs = 0
		M = RE_if.search(file_cache[forF], 0)
		while not M is None:
			_ifs += 1
			if not forF in frm_res.keys(): frm_res[forF] = {}
			frm_res[forF][M.span()] = ("if",)
			xVerb(f"IF: at position {M.span()}")
			M = RE_if.search(file_cache[forF], M.end())
		ifs += _ifs
		file_cache[forF+':IF'] = _ifs
	if ifs != 0: Verb(ifs,"conditional instructions found")

	if len(frm_res.keys()) > 0:
		for forF, frmF in frm_res.items():
			S, keys = f"For: {forF}", sorted([k for k in frmF.keys() if not frmF[k][0] in ("empty","pass","if")])
			if untranslated != 0:
				S += "\nNot translated: "+str(file_cache[forF+':E'])
				del file_cache[forF+':E']
				if where:
					for key in sorted([k for k in frmF.keys() if frmF[k][0] in ("empty","pass")]):
						at = [str(Line(file_cache[forF+':L'], n)) for n in key]
						if frmF[key][0] == "empty":
							S += f"\n! empty translation @ lines {':'.join(at)}"
						else:# frmF[key][0] == "pass":
							S += f"\n! passed translation @ lines {':'.join(at)}"
			if ifs != 0:
				S += "\nConditional instructions: "+str(file_cache[forF+':IF'])
				del file_cache[forF+':IF']
				if where:
					for key in sorted([k for k in frmF.keys() if frmF[k][0] == "if"]):
						at = [str(Line(file_cache[forF+':L'], n)) for n in key]
						S += f"\n! if @ lines {':'.join(at)}"
			for k in keys:
				at = [str(Line(file_cache[forF+':L'], n)) for n in k]
				S += f"\n!@Lines {':'.join(at)}:"
				for frm in frmF[k]:
					if frm[0] == "dialog-id":
						if frm[1] == "name-str":
							S += f" Dialog-identifier possibly untranslated\n=\t{frm[2]}"
						elif frm[1] == "name":
							S += f" Dialog-identifier changed\n<\t{frm[2]}\n>\t{frm[3]}"
						else:# frm[1] == "attr":
							S += f" Dialog-identifier attributes changed\n<\t{frm[2]}\n>\t{frm[3]}"
					elif frm[0] == "post-args":
						if frm[0] == "lost":
							S += f" Dialog post-string arguments: LOST\n<\t{frm[1]}"
						elif frm[0] == "gained":
							S += f" Dialog post-string arguments: GAINED\n>\t{frm[1]}"
						else:# frm[0] == "changed":
							S += f" Dialog post-string arguments: CHANGED\n<\t{frm[1]}\n>\t{frm[2]}"
					else:
						at_pos = [str(n) for n in frm[0]]
						if frm[1] == "Leading white-spaces":
							if frm[2] in ("start","end"):
								S += f"\n\t@ {':'.join(at_pos)}: Leading white-spaces at {frm[2]}:"
							else:
								into = [str(n) for n in frm[2]]
								S += f"\n\t@ {':'.join(at_pos)}: Leading white-spaces at {':'.join(into)}:"
							S += f"\n\t<\t{frm[3]}\n\t>\t{frm[4]}"
						else:
							S += f"\n\t@ {':'.join(at_pos)}: Formating:"
							if not frm[3] is None:
								S += f"\n\t<\t{frm[2]}\n\t>\t{frm[3]}"
							else: S += f"\n\t<\t{frm[2]}\n\t-\tmissing in the translation"
			xVerb(S)
			fPath = os.path.split(forF)
			fName = os.path.splitext(fPath[1])[0]+'.check-info'
			with open(os.path.join(fPath[0],fName), 'w', encoding='utf-8') as f:
				f.write(S)
	if untranslated != 0:
		for k in [f for f in file_cache.keys() if f.endswith(':E')]:
			fPath = os.path.split(k[:-2])
			fName = os.path.splitext(fPath[1])[0]+'.check-info'
			with open(os.path.join(fPath[0],fName), 'w', encoding='utf-8') as f:
				f.write("Not translated: "+str(file_cache[k]))
check.__doc__ = _("""\
forFiles: List - The list of files where translations need to be checked.
untranslated: Integer - Should be: 1 to check empty, 2 to also check passed, or 0 to deactivate.
where: Booleen - Output or not of where the empty translations are.
formats: Booleen - Check or not translations formatting.
         Like leading white-spaces, brackets ([]) and braces ({}).
verbose: Integer - Indicates the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
""")

def fixEmpty(forFiles, /, action='P', *, outdir=None, verbose=0, debug=False):
	Verb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=1 else None
	xVerb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=2 else None
	Debug = lambda *args, **kwargs: print(*args, **kwargs) if debug else None
	if not action in ('P','C','R'):
		Throw(ValueError,_("Invalide argument:"),_("{} parameter should be ").format('action'),'P, C, or R', code=2)
	popFiles = []
	for forF in forFiles:
		if not os.path.isfile(forF):
			Throw(FileNotFoundError,repr(forF),_("doen’t exist or is not a file"))
		if not outdir is None:
			fPath = os.path.split(forF)
			if not os.path.isdir(os.path.join(fPath[0],outdir)):
				os.mkdir(os.path.join(fPath[0],outdir))
			pF = os.path.join(fPath[0],outdir,fPath[1])
			if pF in popFiles: Throw(Exception,_("All files need to be different"))
			popFiles.append(os.path.join(fPath[0],outdir,fPath[1]))
		else:
			if forF in popFiles: Throw(Exception,_("All files need to be different"))
			popFiles.append(forF)

	RE_string = re.compile(reString.format(old=r'old +(?P<old_str>{Q})'.format(Q=reDQ), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
	RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{Q})'.format(N=reN_old, Q=reDQ), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN_new, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
	glob_ppl = 0
	file_cache = {}
	for forF in forFiles:
		Verb('forFile:',repr(forF))
		if not forF in file_cache.keys():
			F, dbg = "", 0
			with open(forF, 'r', encoding='utf-8', newline='') as f:
				r=None
				while r!="":
					r = f.read()
					dbg += 1
					Debug('Debug:read pass',dbg)
					F += r
			file_cache[forF] = F

		pos = ppl = 0
		M, T, M_string, M_dialog = None, 0, RE_string.search(file_cache[forF], pos), RE_dialog.search(file_cache[forF], pos)
		while not (M_string is None and M_dialog is None):
			if not M_string is None and not M_dialog is None:
				if M_dialog.start() < M_string.start():
					M, T = M_dialog, 1
				else:
					M, T = M_string, 0
			elif not M_dialog is None:
				M, T = M_dialog, 1
			else:
				M, T = M_string, 0
			Pass = not RE_pass.match(M.group('new')) is None
			Debug(f'Debug:match span {M.span()} is:\n{M.group()!r}')
			Debug("Debug: new_str group =",repr(M.group('new_str')) if not Pass else 'pass')
			if Pass or M.group('new_str') == '""':
				if action == 'P':
					if T == 0:
						file_cache[forF] = file_cache[forF][:M.start()]+file_cache[forF][M.end():]
						pos = M.start()
						ppl += 1
					elif not Pass and T == 1:
						n_str = M.group().replace(M.group('new'), 'pass')
						file_cache[forF] = file_cache[forF][:M.start()]+n_str+file_cache[forF][M.end():]
						pos = M.start()+len(n_str)
						ppl += 1
				elif action == 'C':
					if T == 0:
						o_str, n_str, pp = M.group('old'), M.group('new'), len(M.group(1))-1
						ind = re.search(r'(\r?\n)([ \t]+){}'.format(re.escape(M.group('new'))), M.group()[pp:]).group(2)
						o_str = re.sub(r'^({I})?'.format(I=ind), r'\1#', o_str)
						n_str = re.sub(r'^({I})?'.format(I=ind), r'\1#', n_str)
						n_str = M.group().replace(M.group('old'), o_str).replace(M.group('new'), n_str)
						file_cache[forF] = file_cache[forF][:M.start()]+n_str+file_cache[forF][M.end():]
						pos = M.start()+len(n_str)
						ppl += 1
					elif not Pass and T == 1:
						pArgs = '' if M.group('new_pArgs') is None else M.group('new_pArgs')
						n_str = M.group().replace(M.group('new')+pArgs, 'pass')
						file_cache[forF] = file_cache[forF][:M.start()]+n_str+file_cache[forF][M.end():]
						pos = M.start()+len(n_str)
						ppl += 1
				elif action == 'R':
					file_cache[forF] = file_cache[forF][:M.start()]+file_cache[forF][M.end():]
					pos = M.start()
					ppl += 1
			else: pos = M.end()
			M_string, M_dialog = RE_string.search(file_cache[forF], pos), RE_dialog.search(file_cache[forF], pos)
		RE_strBlock = re.compile(r'(^|\r?\n)translate +[a-zA-Z]+[a-zA-Z_]* strings *:([ \t]*#[^\r\n]*)?(\r?\n(#[^\r\n]*)?)*(\r?\n([ \t]+#[^\r\n]*)?)*?(?P<TR>\r?\n(##[^\n]*\r?\n)*({rID}\r?\n)?translate |$)'.format(rID=reRID))
		file_cache[forF] = RE_strBlock.sub(r'\g<TR>', file_cache[forF])
		Verb('A total of',ppl,'empty translations was fixes')
		glob_ppl += ppl
	Verb('A total of',glob_ppl,'empty translations was fixes in globality')

	for forF, popF in zip(forFiles, popFiles):
		with open(popF, 'w', encoding='utf-8', newline='') as f:
			f.write(file_cache[forF])
fixEmpty.__doc__ = _("""\
forFiles: List - The list of files where empty translations need to be fixed.
action: Char - Should be: 'P' to change empty translation to the 'pass' keyword, 'C' to also comment
        'strings' translations instead of deleting them, or 'R' to remove all empty or passed translation.
outdir: String - A path relative from the 'forFiles' paths to indicate where to save the result or None
        to save inplace.
verbose: Integer - Indicates the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
""")

def reorder(forFiles, /,*, reverse=False, proxy=False, outdir=None, verbose=0, debug=False):
	Verb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=1 else None
	xVerb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=2 else None
	Debug = lambda *args, **kwargs: print(*args, **kwargs) if debug else None
	if not isinstance(reverse, bool):
		Throw(TypeError,_("Invalide argument:"),_("{} parameter should be of booleen type").format('reverse'), code=2)
	if not isinstance(proxy, bool):
		Throw(TypeError,_("Invalide argument:"),_("{} parameter should be of booleen type").format('proxy'), code=2)
	popFiles = []
	for forF in forFiles:
		if not os.path.isfile(forF):
			Throw(FileNotFoundError,repr(forF),_("doen’t exist or is not a file"))
		if not outdir is None:
			fPath = os.path.split(forF)
			if not os.path.isdir(os.path.join(fPath[0],outdir)):
				os.mkdir(os.path.join(fPath[0],outdir))
			pF = os.path.join(fPath[0],outdir,fPath[1])
			if pF in popFiles: Throw(Exception,_("All files need to be different"))
			popFiles.append(os.path.join(fPath[0],outdir,fPath[1]))
		else:
			if forF in popFiles: Throw(Exception,_("All files need to be different"))
			popFiles.append(forF)

	RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{Q})'.format(N=reN_old, Q=reDQ), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN_new, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
	glob_ppl = 0
	file_cache = {}
	for forF in forFiles:
		Verb('forFile:',repr(forF))
		if not forF in file_cache.keys():
			F, dbg = "", 0
			with open(forF, 'r', encoding='utf-8', newline='') as f:
				r=None
				while r!="":
					r = f.read()
					dbg += 1
					Debug('Debug:read pass',dbg)
					F += r
			file_cache[forF] = F

		buffer = {}
		start,end, pos, ppl = -1,-1, 0, 0
		RE_x = re.compile(reFrm)
		M, M_dialog = None, RE_dialog.search(file_cache[forF], 0)
		while not M_dialog is None:
			M = M_dialog
			if reverse:
				tr_id = (M.group('file'),int(M.group('line')),M.start()) if not M.group('file') is None else None
			else:
				if proxy:
					o_str = M.group('old_str')
					x = RE_x.search(o_str)
					while not x is None:
						if x.group('bkF') is None or not x.group('bkF').isidentifier():
							o_str = o_str[:x.start()]+o_str[x.end():]
							x = RE_x.search(o_str, x.start())
						else: x = RE_x.search(o_str, x.end())
					tr_id = N_str(o_str)
				else: tr_id = M.group('old_str')
			if not tr_id is None:
				if start == -1: start = M.start()
				pos, end = M.start(), M.end()
				if not tr_id in buffer.keys():
					buffer[tr_id] = []
					if not reverse: pos = end
				else:
					file_cache[forF] = file_cache[forF][:M.start()]+file_cache[forF][end:]
				if reverse: file_cache[forF] = file_cache[forF][:M.start()]+file_cache[forF][end:]
				buffer[tr_id].append(M.group())
			else: pos = M.end()
			M_dialog = RE_dialog.search(file_cache[forF], pos)

		if reverse:
			xpos = start
			for tr_key in sorted(buffer.keys()):
				file_cache[forF] = file_cache[forF][:xpos]+buffer[tr_key][0]+file_cache[forF][xpos:]
				xpos += len(buffer[tr_key][0])
		else:
			for tr_key in sorted(buffer.keys()):
				pos = file_cache[forF].find(buffer[tr_key][0])# can never be not found (-1)
				Debug("Debug: pos = ",pos,'\n\t',repr(buffer[tr_key][0]), sep='')
				xpos = pos+len(buffer[tr_key][0])
				for tr_val in buffer[tr_key][1:]:
					file_cache[forF] = file_cache[forF][:xpos]+tr_val+file_cache[forF][xpos:]
					xpos += len(tr_val)

	for forF, popF in zip(forFiles, popFiles):
		with open(popF, 'w', encoding='utf-8', newline='') as f:
			f.write(file_cache[forF])
reorder.__doc__ = _("""\
forFiles: List - The list of files where translations need to be reordered.
reverse: Booleen - Reordering as it was originaly (as extracted by Ren'Py) or process normaly.
proxy: Booleen - Active or not the regroupement in function of their normalized alpha-numeric
       representation.
outdir: String - A path relative from the 'forFiles' paths to indicate where to save the result or None
        to save inplace.
verbose: Integer - Indicates the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging.
""")

def populate(forFiles, *FromFiles, bunch=False, bulk=False, proxy=0, proxyWarn=True, fromNID=False, overwrite='N', outdir=None, verbose=0, debug=False):
	Verb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=1 else None
	xVerb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=2 else None
	Debug = lambda *args, **kwargs: print(*args, **kwargs) if debug else None
	if not isinstance(bunch, bool):
		Throw(TypeError,_("Invalide argument:"),_("{} parameter should be of booleen type").format('bunch'), code=2)
	if not isinstance(bulk, bool):
		Throw(TypeError,_("Invalide argument:"),_("{} parameter should be of booleen type").format('bulk'), code=2)
	if not proxy in (0,1,2,3,4):
		Throw(ValueError,_("Invalide argument:"),_("{} parameter should be an integer between 0 and 4 (includes)").format('proxy'), code=2)
	if not isinstance(proxyWarn, bool):
		Throw(TypeError,_("Invalide argument:"),_("{} parameter should be of booleen type").format('proxyWarn'), code=2)
	if not isinstance(fromNID, bool):
		Throw(TypeError,_("Invalide argument:"),_("{} parameter should be of booleen type").format('fromNID'), code=2)
	if not overwrite in ('N','A','F'):
		Throw(ValueError,_("Invalide argument:"),_("{} parameter should be N for no, A for ask, or F for force").format('override'), code=2)
	popFiles = []
	length = len(FromFiles[0]) if len(FromFiles) > 0 else 0
	if bulk:
		if bunch: bunch = False
		for forF in forFiles:
			if not os.path.isfile(forF):
				Throw(FileNotFoundError,repr(forF),_("doen’t exist or is not a file"))
			if not outdir is None:
				fPath = os.path.split(forF)
				if not os.path.isdir(os.path.join(fPath[0],outdir)):
					os.mkdir(os.path.join(fPath[0],outdir))
				popFiles.append(os.path.join(fPath[0],outdir,fPath[1]))
			else: popFiles.append(forF)
		for fromFiles in FromFiles:
			if length != len(fromFiles):
				Throw(Exception,_("The length of each list of files need to be equal"))
			for fromF in fromFiles:
				if not os.path.isfile(fromF):
					Throw(FileNotFoundError,repr(fromF),_("doen’t exist or is not a file"))
				if fromF in forFiles:
					Throw(Exception,_("Both list of files need to have their files different"))
	else:
		fl = False
		for fromFiles in FromFiles:
			if len(forFiles) != len(fromFiles):
				Throw(Exception,_("The length of both list of files need to be equal"))
			for forF, fromF in zip(forFiles, fromFiles):
				if not os.path.isfile(forF):
					Throw(FileNotFoundError,repr(forF),_("doen’t exist or is not a file"))
				if not os.path.isfile(fromF):
					Throw(FileNotFoundError,repr(fromF),_("doen’t exist or is not a file"))
				if forF == fromF:
					Throw(Exception,_("Both list of files need to have their files different"))
				if not fl and not outdir is None:
					fPath = os.path.split(forF)
					if not os.path.isdir(os.path.join(fPath[0],outdir)):
						os.mkdir(os.path.join(fPath[0],outdir))
					popFiles.append(os.path.join(fPath[0],outdir,fPath[1]))
				elif not fl: popFiles.append(forF)
			fl = True

	RE_string = re.compile(reString.format(old=r'old +(?P<old_str>{Q})'.format(Q=reDQ), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
	RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{Q})'.format(N=reN_old, Q=reDQ), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN_new, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
	RE_x = re.compile(reFrm)
	RE_dID = re.compile(reN_new)
	glob_ppl, group_ppl = 0, 0
	file_cache = {}
	def get_dID(M, M_from):
		dID = None if M.group('new_dID') is None and M.group('dID') is None else M.group('dID') if M.group('new_dID') is None else M.group('new_dID')
		if dID is None: dID = ""
		else:
			_dID = RE_dID.search(dID)
			strID = not re.match(r'^{SQ}|{DQ}$'.format(SQ=reSQ,DQ=reDQ), _dID.group('new_NID')) is None
			if fromNID and strID and not M_from.group('new_dID') is None:
				f_strID = not re.match(r'^{SQ}|{DQ}$'.format(SQ=reSQ,DQ=reDQ), M_from.group('new_NID')) is None
				if f_strID:
					dID = RE_dID.sub(r'{NID}\g<new_idA>'.format(NID=M_from.group('new_NID')), dID)+" "
				else: dID += " "
			else: dID += " "
		return dID
	def _find(With, T, M, into):
		M_from, m_from = None, With.search(file_cache[into])
		Debug("Debug: T=",T," m_from:",repr(m_from))
		if T == 1:
			T_from = 1
			TID = M.group('TID')
			while not m_from is None and (M_from is None or M_from.group('TID') != TID):# get the eventual latest
				if RE_pass.match(m_from.group('new')) is None:
					M_from = m_from
					if m_from.group('TID') == TID: break
				m_from = With.search(file_cache[into], m_from.end())
		else:
			T_from = 0
			while not m_from is None:# get the eventual latest
				M_from = m_from
				m_from = With.search(file_cache[into], m_from.end())
		return (M_from,T_from)
	def _proxysearch(what, fromF):
		T_from = pos = 0
		M, M_string,M_dialog = None, RE_string.search(file_cache[fromF],pos), RE_dialog.search(file_cache[fromF],pos)
		corresp, w = {}, eval(what)# since we now this is string, np
		while not (M_string is None and M_dialog is None):
			if not M_string is None and not M_dialog is None:
				if M_dialog.start() < M_string.start():
					T_from, M = 1, M_dialog
				else:
					T_from, M = 0, M_string
			else:
				T_from, M = (0, M_string) if M_dialog is None else (1, M_dialog)
			Pass = not RE_pass.match(M.group('new')) is None
			if not Pass and M.group('new_str') != '""':
				o_str = eval(M.group('old_str'))# since we now this is string, np
				x = RE_x.search(o_str)
				while not x is None:
					if not x.group('bkF') is None and not x.group('bkF').isidentifier():
						o_str = o_str[:x.start()]+o_str[x.end():]
						x = RE_x.search(o_str, x.start())
					else: x = RE_x.search(o_str, x.end())
				c = cmp(w, o_str)
				if c >= .90: corresp[(c, M.start())] = (T_from, M)
			pos = M.end()
			M_string,M_dialog = RE_string.search(file_cache[fromF],pos), RE_dialog.search(file_cache[fromF],pos)
		return corresp[sorted(corresp.keys())[-1]] if len(corresp.keys()) != 0 else (0,None)
	def _populate(forF, fromF):
		nonlocal glob_ppl, group_ppl
		Verb('forFile:',repr(forF))
		Verb('fromFile:',repr(fromF))
		if not fromF in file_cache.keys():
			F, dbg = "", 0
			with open(fromF, 'r', encoding='utf-8', newline='') as f:
				r=None
				while r!="":
					r = f.read()
					dbg += 1
					Debug('Debug:read pass',dbg)
					F += r
			file_cache[fromF] = F

		pos = ppl = 0
		M, M_from, T, M_string, M_dialog = None, None, 0, RE_string.search(file_cache[forF], pos), RE_dialog.search(file_cache[forF], pos)
		while not (M_string is None and M_dialog is None):
			if not M_string is None and not M_dialog is None:
				if M_dialog.start() < M_string.start():
					M, T = M_dialog, 1
				else:
					M, T = M_string, 0
			elif not M_dialog is None:
				M, T = M_dialog, 1
			else:
				M, T = M_string, 0
			M_from, Pass, by_proxy = None, not RE_pass.match(M.group('new')) is None, False
			Debug(f'Debug:match span {M.span()} is:\n{M.group()!r}')
			Debug("Debug: new_str group =",repr(M.group('new_str')) if not Pass else 'pass')
			if Pass or M.group('new_str') == '""' or overwrite != 'N':
				_RE_dialog = re.compile(reDialog.format(old=re.escape(M.group('old')), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN_new, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
				_RE_string = re.compile(reString.format(old=re.escape(M.group('old')), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
				if T == 1:
					RE_from = _RE_dialog
				else:
					RE_from = _RE_string
				M_from, T_from = _find(RE_from, T, M, fromF)
				if M_from is None and bunch:
					for F in [fF for fF in fromFiles if fF != fromF]:
						M_from, T_from = _find(RE_from, T, M, fromF)
						if not M_from is None: break
				Debug("Debug: M_from:",repr(M_from))
				if M_from is None:
					if proxy > 0:
						Debug("Debug: Proxy 1")
						by_proxy = True
						if T == 1 and not M.group('old_NID') is None:
							reNID_old = r'(?P<old_NID>{NID}( +[a-zA-Z_]+[a-zA-Z_0-9]*)*( +@( +[a-zA-Z_]+[a-zA-Z_0-9]*)+)?)'.format(NID=M.group('old_NID'))
							_RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{old})'.format(N=reNID_old, old=re.escape(M.group('old_str'))), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN_new, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
							TID = M.group('TID')
							m_from = _RE_dialog.search(file_cache[fromF])
							while not m_from is None and (M_from is None or M_from.group('TID') != TID):# get the eventual latest
								if RE_pass.match(m_from.group('new')) is None:
									M_from = m_from
									if m_from.group('TID') == TID: break
								m_from = RE_from.search(file_cache[fromF], m_from.end())
						if M_from is None and proxy > 1:
							Debug("Debug: Proxy 2")
							_RE_string = re.compile(reString.format(old=r'old +(?P<old_str>{old})'.format(old=re.escape(M.group('old_str'))), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
							_RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{old})'.format(N=reN_old, old=re.escape(M.group('old_str'))), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN_new, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
							if T == 1:
								m_from = _RE_dialog.search(file_cache[fromF])
								while not m_from is None:# get the eventual latest
									if RE_pass.match(m_from.group('new')) is None: M_from = m_from
									m_from = _RE_dialog.search(file_cache[fromF], m_from.end())
								if M_from is None:
									T_from = 0
									m_from = _RE_string.search(file_cache[fromF])
									while not m_from is None:# get the eventual latest
										M_from = m_from
										m_from = _RE_string.search(file_cache[fromF], m_from.end())
							if M_from is None and proxy >= 3:
								Debug("Debug: Proxy 3")
								if T == 0:
									T_from = 1
									m_from = _RE_dialog.search(file_cache[fromF])
									while not m_from is None:# get the eventual latest
										if not RE_pass.match(M.group('new')) is None: M_from = m_from
										m_from = _RE_dialog.search(file_cache[fromF], m_from.end())
								else:
									pass# same as for proxy 2
							if M_from is None and proxy >= 4:
								Debug("Debug: Proxy 4")
								T_from, M_from = _proxysearch(M.group('old_str'), fromF)
				if M_from is None: Debug('Debug:from_match span (-1, -1) is:\n""')
				else: Debug(f'Debug:from_match span {M_from.span()} is:\n{M_from.group()!r}')
				if not M_from is None and M_from.group('new_str') == '""': M_from = None
			if not M_from is None:
				if T_from == 1:# to ensure all groups are present
					M_from = RE_dialog.search(file_cache[fromF], M_from.start())
					Debug("Debug: M is",repr(M),"\n  M_from is",repr(M_from))
				A = None
				if (not by_proxy and not Pass and not M.group('new_str') in ('""', M_from.group('new_str')) and overwrite == 'A') or (by_proxy and proxyWarn and (Pass or M.group('new_str') == '""')):
					pp = len(M.group(1))-1
					sPy = ""
					if T_from == 1:
						prePy, Py = M_from.group('prePy'), M_from.group('Py')
						mPy = RE_Py.search(prePy)
						while not mPy is None:
							if not mPy.group() in M.group(): sPy += mPy.group()+'\n\t'
							mPy = RE_Py.search(prePy, mPy.end())
						mPy = RE_Py.search(Py)
						while not mPy is None:
							if not mPy.group() in M.group(): sPy += mPy.group()+'\n\t'
							mPy = RE_Py.search(Py, mPy.end())

					if T == 1:
						o_sPy = ""
						prePy, Py = M.group('prePy'), M.group('Py')
						mPy = RE_Py.search(prePy)
						while not mPy is None:
							if not mPy.group() in sPy: o_sPy += mPy.group()+'\n\t'
							mPy = RE_Py.search(prePy, mPy.end())
						mPy = RE_Py.search(Py)
						while not mPy is None:
							if not mPy.group() in sPy: o_sPy += mPy.group()+'\n\t'
							mPy = RE_Py.search(Py, mPy.end())
						if T_from == 1:
							print(_("For: {N_old}\n\t{old}\nAttempt to replace: {N_new}\n\t{new}\nWith: {N_From}\n\t{From}").format(N_old=M.group('dID'),old=M.group('old_str'), N_new=M.group('new_dID') if not Pass else '', new=o_sPy+M.group('new_str') if not Pass else 'pass', N_From=M_from.group('new_dID'), From=sPy+M_from.group('new_str')))
						else:
							print(_("For: {N_old}\n\t{old}\nAttempt to replace: {N_new}\n\t{new}\nWith:\n\t{From}").format(N_old=M.group('dID'),old=M.group('old_str'), N_new=M.group('new_dID') if not Pass else '', new=o_sPy+M.group('new_str') if not Pass else 'pass', From=sPy+M_from.group('new_str')))
					else:
						if T_from == 1:
							print(_("For:\n\t{old}\nAttempt to replace:\n\t{new}\nWith: {N_From}\n\t{From}").format(old=M.group('old_str'), new=M.group('new_str') if not Pass else 'pass', N_From=M_from.group('new_dID'), From=M_from.group('new_str')))
						else:
							print(_("For:\n\t{old}\nAttempt to replace:\n\t{new}\nWith:\n\t{From}").format(old=M.group('old_str'), new=M.group('new_str') if not Pass else 'pass', From=M_from.group('new_str')))
					if by_proxy: print(_("Warning:"),_("Please note that the 'With' was found by proxy."))
					while not A:
						A = input(_("Proceed?")+" (N|Y) >>> ")
						if A.upper() in ('Y','YES'): A='P' if by_proxy else 'Y'
						elif A.upper() in ('N','NO'): A='N'
						else: A=None
				if ((not by_proxy or not proxyWarn or A == 'P') and (Pass or M.group('new_str') == '""')) or (not by_proxy and (M.group('new_str') != M_from.group('new_str') and (overwrite == 'F' or A == 'Y'))):
					pp = len(M.group(1))-1
					nl = re.search(r'(\r?\n)([ \t]+){}'.format(re.escape(M.group('new'))), M.group()[pp:])
					nl,ind = nl.group(1),nl.group(2)
					sPy = ""
					if T_from == 1:
						prePy, Py = M_from.group('prePy'), M_from.group('Py')
						mPy = RE_Py.search(prePy)
						while not mPy is None:
							if not mPy.group() in M.group(): sPy += mPy.group()+nl+ind
							mPy = RE_Py.search(prePy, mPy.end())
						mPy = RE_Py.search(Py)
						while not mPy is None:
							if not mPy.group() in M.group(): sPy += mPy.group()+nl+ind
							mPy = RE_Py.search(Py, mPy.end())

					if not Pass:
						if (T, T_from) == (1, 1):
							dID = get_dID(M, M_from)
							xF = file_cache[forF][M.start()+pp:].replace(M.group('new'), sPy+dID+M_from.group('new_str'), 1)
						else:
							xF = file_cache[forF][M.start()+pp:].replace(M.group('new_str'), M_from.group('new_str'), 1)
					else:
						Debug("<<< M old is Pass\n<<< From new is",repr(M_from.group('new_str')))
						if fromNID and T_from == 1:
							dID = get_dID(M, M_from)
						else:
							dID = "" if M.group('dID') is None else M.group('dID')+" "
						pArgs = '' if M.group('old_pArgs') is None else M.group('old_pArgs')

						xF = file_cache[forF][M.start()+pp:].replace(M.group('new'), sPy+dID+M_from.group('new_str')+pArgs+nl, 1)
					xVerb('Change translation from',repr(M.group('new_str') if not Pass else M.group('old_str')),'\tto',repr(M_from.group('new_str')))
					file_cache[forF] = file_cache[forF][:M.start()+pp]+xF
					ppl += 1
				else: M_from = None
			pos = M.end()-(len(M.group('new'))-len(M_from.group('new')) if not M_from is None else 0)
			M_string, M_dialog = RE_string.search(file_cache[forF], pos), RE_dialog.search(file_cache[forF], pos)
		Verb('A total of',ppl,'translations was populate')
		group_ppl += ppl
		glob_ppl += ppl
		with open(forF, 'w', encoding='utf-8', newline='') as f:
			f.write(file_cache[forF])

	for forF, popF in zip(forFiles, popFiles):
		Debug('forFile:',repr(forF))
		if not forF in file_cache.keys():
			F, dbg = "", 0
			with open(forF, 'r', encoding='utf-8', newline='') as f:
				r=None
				while r!="":
					r = f.read()
					dbg += 1
					Debug('Debug:read pass',dbg)
					F += r
			file_cache[popF] = F
	for fromFiles in FromFiles:
		group_ppl = 0
		if bulk:
			for popF in popFiles:
				for fromF in fromFiles:
					_populate(popF, fromF)
		else:
			for popF, fromF in zip(popFiles, fromFiles):
				_populate(popF, fromF)
		Verb('A total of',group_ppl,'translations was populate with this group of from-files')
	Verb('A total of',glob_ppl,'translations was populate in globality')
populate.__doc__ = _("""\
forFiles: List - The list of files that need to be populated.
FromFiles: List,... - Successions of list of files from where to get the translations.
bunch: Booleen - Indicates if, when a translation is not found, it should be searched for in other files
       in the current fromFiles group.
bulk: Booleen - Indicates if few files should be populate from many (or many from few).
proxy: Integer - From 0 to 4 (includes) to indicate a level of proxy, 0 to diactivate, 4 made it long.
proxyWarn: Booleen - Indicates whether an asking is made or not when attempting to populate empty
           translation with proxied translation.
fromNID: Booleen - Indicates whether the identifiant name’ string is prefered to be from 'fromFiles' or from
         'forFiles'.
overwrite: Char - Should be: 'N' for no, 'A' for ask, or 'F' for force. (verbose is recommand with 'A')
outdir: String - A path relative from the 'forFiles' paths to indicate where to save the result or None
        to save inplace.
verbose: Integer - Indicates the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
""")

if __name__ == '__main__':
	Cmd, verbose, dbg = None, 0, False
	Cmds = ('populate','fix-empty','check','diff','reorder','--help')
	Cmd = sys.argv.pop(1)
	if not Cmd in (*Cmds,'-h'):
		Error(_("Invalide argument:"),_("the 1st argument need to one of the following:"),', '.join(Cmds),_("\n  not"),Cmd)
	if Cmd in ('-h','--help'):
		print(dedent(_("""\
		python3 -m rpyTranslations ({cmds}) [args] [options]

		Please note that the parentesis are just to englob all of the possible command and should not be put.

		-h, --help
		  Show this general help and exit.
		  Enter a command followed the help one to see their specific help.
		  Please note that help commands obviousely doesn’t accept any args or options even if they still
		   show the requested help.
		args
		  Their are specific for each command and generaly should be put in a specific order.
		  See the specific help of these commands for more information.
		options
		  They generaly can be put everywhere in the argument realm of command.
		  However, though they can be put in random order, they are treated by order of precedence and each
		   option can be specified only one times.
		  The precedence for an option is in right to left order (so, short version of options are always last).
		  Exemple, for `-% n, --multi n, --lists n`, the precedence is:
		      `--lists n` > `--multi n` > `-% n`
		  So if we have this: `-% 2 --multi 5 --lists 3`, only `--list 3` will be treated and retained and the
		   remaining will stay as is.
		  Some commands have inter-option precedences. This means that each options will be treated but only
		   the one with the greater precedence will be retained (see the specific helps for more details).

		  Though commands has their own set of options, here are some general options below:
		  --
		      This option allow to indicate where to stop interpreting options.
		      Exemple, in the following: `-d -- -v`, -d will activates the debug but -v will activates nothing
		       and stay as -v.
		  -:
		      This option should be directly followed (without spaces) by any characters.
		      These characters should each correspond to short version of options that do not requiered
		       arguments.
		      Exemple: `-:vd` correponding to `-v -d`.
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process.
		      Please note that the output can be huge.
		  See the specific help of commands to see the other options available.
		""").format(cmds='|'.join(Cmds))))
		if len(sys.argv) != 1:
			Error(_("Invalide argument:"),_("help commands do not accept any argument or option."))
		exit()
	elif Cmd == 'populate' and sys.argv[1] in ('-h','--help'):
		print(dedent(_("""\
		rpyTranslations populate [--help] for_files... from_files... [options]

		for_files and from_files are successions of files and both need to be of equal length.
		for_files are the reference files where the translations need to be populate.
		from_files are the reference files from where the translations is get.

		options:
		  -h, --help
		      Show this specific help for the 'populate' command and exit.
		  -b, --bunch
		      Indicates if, when a translation is not found, it should be searched for in other files in the
		       current from-files group.
		  --bulk [n]
		      Indicates if few files should be populate from many (or many from few).
		      If n is puts, it indicate the number of file in the for_files group (default 1).
		  -o dir, --subdir dir
		      If put, it indicates a sub-directory to where the populated translation files are saved,
		       otherwise on the for-files themself.
		  -% n, --multi n, --lists n
		      This allow to give the number of list from where merging multiple translations. This include
		       the first group of files (the for_files).
		      So, for 2 from_files, you should put it with 3.
		      It defaults to 2 unless --bulk is use, in that case it defaults to 1 because this latter control the
		       first list of files.
		      The internal workflow is like:
		          for_files - from_files[0] > pop_files ; pop_files - from_files[1] > pop_files ; ...
		          * Where from_files[#] are group of files of the same length as forFiles.
		  -a, --ask
		      If put, a prompt will ask to confirm or no a replacement of translation (only for non-empty).
		      It’s recommand to use the verbose option with that, so you can know for what file the asking is.
		  -F, --force, --overwrite
		      Activate the replacement of existing translations.
		  --proxy lvl
		      If a translation is not found with the normals methods, so this option allow to use other methods
		       based on the proxymity level (lvl).
		      lvl can be one of the following:
		          0   Default. No proxy maked.
		          1   For 'dialogs', allow to focus only in the name part of the dialog identifier.
		          2   For 'dialogs', allow to fully ignore the dialog identifier and as fallback to search in
		               'strings' translations, but not allow for 'strings' to search in 'dialogs' translations.
		          3   Allow to focus only on the strings. With that, 'dialogs' can now be considered as same
		               as 'strings', so 'strings' can now search in 'dialogs' translations.
		          4   The strings are compared to get a least a correspondance of 90%.
		              Please note that this can be a very long process.
		  -W, --no-proxy-warn
		      When attempting to populate empty translation with proxied translation, an asking is made.
		      This option allow to bypass this asking.
		  --proxy-y lvl
		      This option is the same as --proxy but with effect of --no-proxy-warn in addition.
		  -N, --from-nID
		      By default, the identifiant name’ string are takes from for_files, use this option if rather
		       prefere it’s to be from from_files.
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process. The output can be huge.
		* Please note that --ask take precedence over --force.
		* Please note that --proxy take precedence over --proxy-y (though the side effect of this one is kept)
		   and that --no-proxy-warn had no effect if --proxy-y is use too.

		Please note that `populate --bulk 2 ./common.rpy ./init.rpy -% 1 ../from/common.rpy ../from/init.rpy`
		 result to the same as `populate --bunch ./common.rpy ./init.rpy ../from/common.rpy ../from/init.rpy`
		 but are less efficiency.
		""")),"   "+'-'*104,dedent(_("""
		Additional informations:

		- About the --ask option:
		    The patern to show askings is the following:
		      `
		      For: [for: old dID]
		      	<for: old string>
		      Attempt to replace: [for: new dID]
		      	<for: new string>
		      With: [with: dID]
		      	<with: string>
		      `
		    The presence of dID parts depends of the translation’ kind (string or dialog translation).
		    Only dialog translations can contain a dID, and if one doesn't have one, it will be showed as {0}.
		    It’s important to note that the dID contains two parts, the first part is a NID (name identifiant)
		     and the second is image arguments.
		    As mentioned above, only the NID can be affect by the --from-nID option, this means that first, the
		     dID is get in the following order of precedence:
		      [for: new dID] > [for: old dID]
		     and then, if the --from-nID option is use, the NID can be take from the [with: dID] if:
		      1) the previously get dID is not {0} and it NID is of string type
		      2) this [with: dID] is not {0} and it NID is of string type
		     otherwise, the NID from the previously get dID is keep.
		""").format(repr(None))), sep='\n')
		if len(sys.argv) != 2:
			Error(_("Invalide argument:"),_("help commands do not accept any argument or option."))
		exit()
	elif Cmd == 'fix-empty' and sys.argv[1] in ('-h','--help'):
		print(dedent(_("""\
		rpyTranslations fix-empty [--help] for_files... [options]

		for_files are the files where the empty translations need to be fix.

		options:
		  -h, --help
		      Show this specific help for the 'fix-empty' command and exit.
		  -a act, --action act
		      This option allow to specify which action take when empty strings are found.
		      act should be one of the following:
		          P   Default. All empty 'dialogs' translation are replaced by the 'pass' keyword and the
		               empty 'strings' translation are removed.
		          C   All empty 'dialogs' translation are replaced by the 'pass' keyword and the empty
		               'strings' translation are commented.
		          R   All empty translation are removed. Please note that this also remove 'dialogs'
		               translation set with the 'pass' keyword.
		  -o dir, --subdir dir
		      If put, it indicates a sub-directory to where the fixed translation files are saved, otherwise
		       on the for-files themself.
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process. The output can be huge.
		""")))
		if len(sys.argv) != 2:
			Error(_("Invalide argument:"),_("help commands do not accept any argument or option."))
		exit()
	elif Cmd == 'check' and sys.argv[1] in ('-h','--help'):
		print(dedent(_("""\
		rpyTranslations check [--help] for_files... [options]

		for_files are the files where to check the translations.

		options:
		  -h, --help
		      Show this specific help for the 'check' command and exit.
		  -n, --skip-empty
		      Deactivate checkings for empty translations.
		  -e, --empty
		      Default. Check how many translation is still empty.
		  -p, --not-translate
		      Check how many translation is still empty or are set with the 'pass' keyword.
		  --where
		      Output or not of where the empty translations are.
		      Please note that this option had no effect if put with the --skip-empty.
		  -f, --format
		      Sometimes leading white-spaces can be typos but generaly they are on purpose (especialy when an
		       'extend' is in use).
		      Check if the translations have keep all kinds of formating and that leading white-spaces are
		       respected.
		      * Formatings like brackets ([]) and braces ({}).
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process. The output can be huge.
		* Please note that --skip-empty take precedence over --not-translate that take precedence over --empty.
		""")))
		if len(sys.argv) != 2:
			Error(_("Invalide argument:"),_("help commands do not accept any argument or option."))
		exit()
	elif Cmd == 'diff' and sys.argv[1] in ('-h','--help'):
		print(dedent(_("""\
		rpyTranslations diff [--help] for_files... [options]

		for_files and from_files are successions of files and both need to be of equal length.
		for_files are the files where differences of translation need to be compared.
		from_files are the reference files from where other translations is get for comparing.

		Since this command is for comparing adds and removing in the for-files, there is no proxy option, and
		 obviousely, empty translation and the 'pass' keyword are treat as different.

		options:
		  -h, --help
		      Show this specific help for the 'diff' command and exit.
		  --ignore-newpart
		      Indicates if the comparisons of new-part of translations should be output or not.
		  --what
		      Indicates if what is add or is no longer present in the translation files should be output or not.
		  --reflines
		      Indicates if the comparisons of reference-lines should be output or not.
		  --tr-id
		      Indicates if the comparisons of translation-identifiers should be output or not.
		  -% n, --multi n, --lists n
		      This allow to give the number of list from where other translations are get. This include the
		       first group of files (the for_files).
		      So, for 2 from_files, you should put it with 3.
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process. The output can be huge.
		""")))
		if len(sys.argv) != 2:
			Error(_("Invalide argument:"),_("help commands do not accept any argument or option."))
		exit()
	elif Cmd == 'reorder' and sys.argv[1] in ('-h','--help'):
		print(dedent(_("""\
		rpyTranslations reorder [--help] for_files... [options]

		for_files are the reference files where the translations need to be reordered.

		This command try regrouping similiar translations to ease translations and comprisons of equivalant
		 translations.
		Since 'strings' translations are generaly unique, it should be note that it’s only work for 'dialogs'.
		Also, all occurences are regrouping to the first occurence place.

		options:
		  -h, --help
		      Show this specific help for the 'reorder' command and exit.
		  -o dir, --subdir dir
		      If put, it indicates a sub-directory to where the reordered translation files are saved,
		       otherwise on the for-files themself.
		  -r, --reverse
		      If put, try to reordering as it was originaly (as extracted by Ren'Py) through the
		       reference-line comments (# dir/file.rpy:line).
		      This functionality ignoring translation that are not have a reference-line comment, those
		       selected to be reordered are joined and can be inserted between two group of these
		       ignored translations.
		  --proxy
		      If put, this option make translations to be evaluated with a proxy method.
		      With this method, translations are regrouped function of their normalized alpha-numeric
		       representation.
		      Please note that this option had no effect if put with the --reverse option.
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process.
		""")))
		if len(sys.argv) != 2:
			Error(_("Invalide argument:"),_("help commands do not accept any argument or option."))
		exit()
	argv, opt_i,stopO, args,kargs = [], 0,-1, [],{}
	def get_opt(opts, var, val=None, arg_t=None, err=False):
		global argv, opt_i, stopO
		for opt in opts:
			if opt in argv:
				pos = argv.index(opt)
				if stopO < 0 or pos < stopO:
					if not val is None:
						globals().update([(var, val)])
					if not arg_t is None:
						try:
							v = argv[pos+1]
							v = arg_t(v)
							argv.pop(pos+1)
							globals().update([(var, v)])
							if stopO > 1: stopO -= 2
						except IndexError as e:
							if err:
								Error(_("Missing argument:"),_("{} option required an argument.").format(opt))
						except:
							if err:
								if arg_t == int:
									Error(_("Invalide argument:"),_("{} option should be an integer, give:").format(opt),repr(v))
							if stopO > 0: stopO -= 1
					elif stopO > 0: stopO -= 1
					return argv.pop(pos)
	for v in sys.argv[1:]:
		if v.startswith('-:'):
			for a in v[2:]: argv.append('-'+a)
		else: argv.append(v)
	if '--' in argv:
		stopO = argv.index('--')
		argv.remove('--')
	get_opt(['--verbose','-v'], 'verbose', 1, int)
	get_opt(['--debug','-d'], 'dbg', True)
	if dbg: print("Debug: args =",argv)
	if Cmd == 'populate':
		O, D, N, Bc, Bk, fID, proxy, pxW = 'N', None, 2, False, 0, False, 0, True
		get_opt(['--bunch','-b'], 'Bc', True)
		r = get_opt(['--overwrite','--force','-F','-f'], 'O', 'F')
		if r == '-f':
			print("Warning: -f option of populate is now deprecated in favor -F")
			input("<press ENTER to continue>")
		get_opt(['--ask','-a'], 'O', 'A')
		get_opt(['--subdir','-o'], 'D', arg_t=str)
		get_opt(['--bulk'], 'Bk', 1, int)
		if Bk != 0: N = 1
		get_opt(['--lists','--multi','-%'], 'N', arg_t=int, err=True)
		r = get_opt(['--proxy-y'], 'proxy', arg_t=int, err=True)
		if r != None: pxW = False
		get_opt(['--proxy'], 'proxy', arg_t=int, err=True)
		get_opt(['--no-proxy-warn','-W'], 'pxW', False)
		get_opt(['--from-nID','-N'], 'fID', True)
		argc = len(argv)
		if Bk < 0:
			Error(_("Invalide argument:"),_("{} option should be an absolute integer").format('--bulk'), code=2)
		if Bk == 0:
			if argc%N != 0:
				Error(_("The file pair lists need to have equal length, {N} gived.", argc).format(N=argc))
			N = argc//N
			files1 = argv[:N]
		else:
			files1 = argv[:Bk]
			argc -= Bk
			if argc%N != 0:
				Error(_("The file pair lists need to have equal length, {N} gived.", argc).format(N=argc))
			N = argc//N
		files2 = []
		for i,f in enumerate(argv[len(files1):]):
			if i%N == 0: files2.append([f])
			else: files2[-1].append(f)
		Bk = Bk != 0
		if dbg: print(f"Debug: populate({files1}, {files2}, bunch={Bc}, bulk={Bk}, proxy={proxy}, proxyWarn={pxW}, fromNID={fID}, overwrite={O!r}, outdir={D!r}, verbose={verbose}, debug={dbg})")
		args,kargs = [files1,*files2], {'bunch':Bc,'bulk':Bk,'proxy':proxy,'proxyWarn':pxW,'fromNID':fID, 'overwrite':O,'outdir':D,'verbose':verbose,'debug':dbg}
	elif Cmd == 'fix-empty':
		A, D = 'P', None
		get_opt(['--action','-a'], 'A', arg_t=str)
		get_opt(['--subdir','-o'], 'D', arg_t=str)
		files = argv[:]
		if dbg: print(f"Debug: fixEmpty({files}, action={A!r}, outdir={D!r}, verbose={verbose}, debug={dbg})")
		args,kargs = [files], {'action':A,'outdir':D,'verbose':verbose,'debug':dbg}
	elif Cmd == 'check':
		UnT, F, Wr = 1, False, False
		get_opt(['--empty','-e'], 'UnT', 1)
		get_opt(['--not-translate','-p'], 'UnT', 2)
		get_opt(['--where'], 'Wr', True)
		get_opt(['--skip-empty','-n'], 'UnT', 0)
		get_opt(['--format','-f'], 'F', True)
		files = argv[:]
		if dbg: print(f"Debug: check({files}, untranslated={UnT}, where={Wr}, formats={F!r}, verbose={verbose}, debug={dbg})")
		args,kargs = [files], {'untranslated':UnT,'where':Wr,'formats':F,'verbose':verbose,'debug':dbg}
	elif Cmd == 'diff':
		N, nP, Wt, refL, trID = 2, True, False, False, False
		get_opt(['--lists','--multi','-%'], 'N', arg_t=int, err=True)
		get_opt(['--ignore-newpart'], 'nP', False)
		get_opt(['--what'], 'Wt', True)
		get_opt(['--reflines'], 'refL', True)
		get_opt(['--tr-ids'], 'trID', True)
		argc = len(argv)
		if argc%N != 0:
			Error(_("The file pair lists need to have equal length, {N} gived.", argc).format(N=argc))
		N = argc//N
		files1 = argv[:N]
		files2 = []
		for i,f in enumerate(argv[N:]):
			if i%N == 0: files2.append([f])
			else: files2[-1].append(f)
		if dbg: print(f"Debug: diff({files1}, {files2}, newpart={nP}, what={Wt}, reflines={refL}, trID={trID}, verbose={verbose}, debug={dbg})")
		args,kargs = [files1,*files2], {'newpart':nP,'what':Wt,'reflines':refL,'trID':trID, 'verbose':verbose,'debug':dbg}
	elif Cmd == 'reorder':
		D, R, P = None, False, False
		get_opt(['--subdir','-o'], 'D', arg_t=str)
		get_opt(['--reverse','-r'], 'R', True)
		get_opt(['--proxy'], 'P', True)
		files = argv[:]
		if dbg: print(f"Debug: reorder({files}, reverse={R}, proxy={P}, outdir={D!r}, verbose={verbose}, debug={dbg})")
		args,kargs = [files], {'outdir':D,'reverse':R,'proxy':P,'verbose':verbose,'debug':dbg}
	# print("Debug: STOP")
	# exit()
	try:
		if Cmd == 'populate':
			populate(*args, **kargs)
		elif Cmd == 'fix-empty':
			fixEmpty(*args, **kargs)
		elif Cmd == 'check':
			check(*args, **kargs)
		elif Cmd == 'diff':
			diff(*args, **kargs)
		elif Cmd == 'reorder':
			reorder(*args, **kargs)
	except Exception as exc:
		if len(exc.args) == 2: Error(exc.args[0], code=exc.args[1])
		import traceback
		traceback.print_tb(sys.exc_info()[-1], file=SP)
		Error(_("An error occured:\n"),SP(), f"\r{type(exc).__qualname__}:",exc.args[0])
