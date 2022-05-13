#!/usr/bin/env python3
import sys, os, os.path, re, locale, gettext
from unicodedata import normalize as NZ, is_normalized as isNZ
from textwrap import dedent
__all__=['populate','fixEmpty','check','reorder','diff']

TR, LANG = None, locale.getdefaultlocale()
CSET=LANG[1]
LANG=(LANG[0],LANG[0].split('_')[0])
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
	return L

# Old strings should always be double-quote strings such as extracted by Ren'Py, new strings can be otherwise.
reDQ = r'"(\\"|[^"])*"'
reSQ = r"'(\\'|[^'])*'"
reTDQ = r'"""(\\"|""?[^"]|[^"])*"""'
reTSQ = r"'''(\\'|''?[^']|[^'])*'''"
reQ = r'('+reTDQ+r'|'+reTSQ+r'|'+reDQ+r'|'+reSQ+r')'
reP = r'(?P<p>_p\()?{Q}(?(p)\))'.format(Q=reQ)
reN = r'[a-zA-Z_]+[a-zA-Z_0-9]*( +[a-zA-Z_]+[a-zA-Z_0-9]*)*( +@( +[a-zA-Z_]+[a-zA-Z_0-9]*)+)?'
rePy = r'\$(\\\r?\n|[^\n])*'# does not support logical line for '(', '{' and '[' and reQ
rePass = r'pass( *#[^\n]*|\r?\n)'
reRID = r'# (?P<file>[^;:\\/]+(/[^;:\\/]+)*\.rpy):(?P<line>0|[1-9][0-9]*)'

reString = r'^([ \t]+##[^\n]*\r?\n)*([ \t]+{rID}\r?\n)?([ \t]+(?P<old>{old})(\r?\n([ \t]+##[^\n]*)?)*)[ \t]+(?P<new>{new})(\r?\n[ \t]+##?[^\n]*)*\r?\n(\r?\n)?'
reStringCmt = r'^([ \t]+##[^\n]*\r?\n)*([ \t]+{rID}\r?\n)?([ \t]+#(?P<old>{old})(\r?\n([ \t]+##[^\n]*)?)*)[ \t]+#(?P<new>{new})(\r?\n[ \t]+##?[^\n]*)*\r?\n(\r?\n)?'# This is mainly to use with the 'diff' command
reDialog = r'^(##[^\n]*\r?\n)*({rID}\r?\n)?(translate +[a-z]+ +([a-zA-Z_]+[a-zA-Z_0-9]*) *:\r?\n((\s*##[^\n]*)*\s*)*[ \t]+(?P<old>{old})(\r?\n([ \t]+##[^\n]*|[ \t]+{py})*|[ \t]*)*)[ \t]+(?P<new>{new})(\r?\n([ \t]+##?[^\n]*|[ \t]*))*\r?\n(\r?\n)?'
# The final `(\r?\n)?` is to ease and enance the work of 'fixEmpty' and 'reorder' functions.

RE_S = re.compile(r'\s+')# for splittings
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
	s = len(s1)-len(s2)
	if s < 0: s1 += ['']*(-s)
	elif s > 0: s2 += ['']*s
	for c1 in s1:
		_1 = ''.join([chr(c) for c in c1 if chr(c).isascii() and chr(c).isalnum()])
		count += len(c1)
		for c2 in s2[i:]:
			_2, i = ''.join([chr(c) for c in c2 if chr(c).isascii() and chr(c).isalnum()]), i+1
			if _1 == _2:
				score += len(c2)
				break
	return score/count
cmp.__doc__ = _("""This function try to estimate the proxymity between two strings.
Please note that this can hardly be precise and error-free.
""")

def diff(forFiles, *FromFiles, verbose=0, debug=False):
	Verb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=1 else None
	xVerb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=2 else None
	Debug = lambda *args, **kwargs: print(*args, **kwargs) if debug else None
	for fromFiles in FromFiles:
		if len(forFiles) != len(fromFiles):
			Throw(Exception,_("The length of both list of files need to be equal"))
		for forF, fromF in zip(forFiles, fromFiles):
			if not os.path.isfile(forF):
				Throw(FileNotFoundError,repr(forF),_("doen't exist or is not a file"))
			if not os.path.isfile(fromF):
				Throw(FileNotFoundError,repr(fromF),_("doen't exist or is not a file"))
			if forF == fromF:
				Throw(Exception,_("Both list of files need to have their files different"))

	RE_string = re.compile(reString.format(old=r'old +(?P<old_str>{Q})'.format(Q=reDQ), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
	RE_stringCmt = re.compile(reStringCmt.format(old=r'old +(?P<old_str>{Q})'.format(Q=reDQ), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
	RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{Q})'.format(N=reN, Q=reDQ), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
	file_cache, frm_res = {}, {}
	for fromFiles in FromFiles:# file caching loop
		for forF, fromF in zip(forFiles, fromFiles):
			Debug('forFile:',repr(forF))
			if not forF in file_cache.keys():
				F = ""
				with open(forF, 'r', newline='') as f:
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
				with open(fromF, 'r', newline='') as f:
					L, file_cache[forF+':L'] = 0, []
					for line in f:
						L += len(line)
						file_cache[forF+':L'].append(len(line))
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
			pos = 0
			M, M_from, T, M_string, M_stringCmt, M_dialog = None, None, 0, RE_string.search(file_cache[fromF], pos), RE_stringCmt.search(file_cache[fromF], pos), RE_dialog.search(file_cache[fromF], pos)
			while not (M_string is None and M_stringCmt is None and M_dialog is None):
				M, T = get_M(M_string, M_dialog, M_stringCmt)
				M_from, T_from, Pass = None, 0, not RE_pass.match(M.group('new')) is None
				Debug(f'Debug:match span {M.span()} is:\n{M.group()!r}')
				Debug("Debug: new_str group =",repr(M.group('new_str')) if not Pass else 'pass')
				if T == 1:
					_RE_dialog = re.compile(reDialog.format(old=re.escape(M.group('old')), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
					M_from = m_from = _RE_dialog.search(file_cache[forF])
					T_from = 1
					while not m_from is None and m_from.span() in _for_spans:
						m_from = _RE_dialog.search(file_cache[forF], m_from.end())
						if not (m_from is None or m_from.span() in _for_spans):
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
					res[0].append((*M.span(),M.group()))
				else:
					_for_spans.append(M_from.span())
					_res = []
					if not M.group('file') is None:
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
					if T == 2:
						if T_from == 0:
							xVerb("As been uncommented:",repr(M.group('old')))
							_res.append(("commented",False))
					elif T == 0:
						if T_from == 2:
							xVerb("As been commented:",repr(M.group('old')))
							_res.append(("commented",True))
					if Pass:
						if RE_pass.match(M_from.group('new')) is None:
							xVerb("As been unset from pass:",repr(M.group('old')))
							_res.append(("pass",False))
					elif M.group('new') != M_from.group('new'):
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
							rx=r'({{(\\}}|}?[^}])*}})|({(\\}|[^}])*})|(\[(\\\]|[^]])*\])'
							RE_x = re.compile(r'\s*{x}\s*'.format(x=rx))
							o,n, n_end = RE_x.search(o_str),RE_x.search(n_str), 0
							while not o is None:
								if n is None:
									_res.append((o.span(),"Formating",repr(o.group()),None))
									D += 1
								elif o.group() != n.group():
									n_end = n.end()
									_o,_n = re.search(rx, o.group()),re.search(rx, n.group())
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
						if D == 0:# another kind of difference
							_res.append(("diff",M.group('new'),M_from.group('new')))
					if _res != []:
						res[0].append((*M.span(),_res))
				pos = M.end()
				M_string, M_stringCmt, M_dialog = RE_string.search(file_cache[fromF], pos), RE_stringCmt.search(file_cache[fromF], pos), RE_dialog.search(file_cache[fromF], pos)
			# checking for added ones
			pos = 0
			M, T, M_string, M_stringCmt, M_dialog = None, 0, RE_string.search(file_cache[forF], pos), RE_stringCmt.search(file_cache[forF], pos), RE_dialog.search(file_cache[forF], pos)
			while not (M_string is None and M_stringCmt is None and M_dialog is None):
				M, T = get_M(M_string, M_dialog, M_stringCmt)
				if M.span() in _for_spans:
					pos = M.end()
					M_string, M_stringCmt, M_dialog = RE_string.search(file_cache[forF], pos), RE_stringCmt.search(file_cache[forF], pos), RE_dialog.search(file_cache[forF], pos)
					continue
				Pass = not RE_pass.match(M.group('new')) is None
				# _for_spans contain all matching span with fromFile, so if we're here we already now it's add
				if Pass:
					xVerb("Set to pass but added:",repr(M.group('old')))
					res[1].append((*M.span(),"passed"))
				elif T == 3:
					xVerb("Commented but added:",repr(M.group('old')))
					res[1].append((*M.span(),"commented"))
				elif M.group('new_str') == '""':
					xVerb("Empty but added:",repr(M.group('old')))
					res[1].append((*M.span(),"empty"))
				else:
					xVerb("Added:",repr(M.group('old')))
					res[1].append((*M.span(),"added",M.group('new')))
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
					S += f"\n+@Lines {':'.join(at)}:"
					if k[2] == "added":
						S += f" ADDED\n\t{k[3]}"
					elif k[2] == "passed":
						S += " ADDED but set with 'pass'"
					else:
						S += f" ADDED but {k[2]}"
			if len(frmF[fromF][0]) > 0:
				for k in frmF[fromF][0]:
					at = [str(Line(file_cache[forF+':L'], n)) for n in k[:2]]
					if isinstance(k[2], str):
						S += f"\n-@Lines {':'.join(at)}: REMOVED"
					else:
						for frm in k[2]:
							S += f"\n-@Lines {':'.join(at)}:"
							if frm[0] == "reference-line":
								if frm[1] == "file":
									S += f" Reference-line file changed\n\tFrom: {frm[2]} To: {frm[3]}"
								elif frm[1] == "line":
									S += f" Reference-line line changed\n\tFrom: {frm[2]} To: {frm[3]}"
								else:# frm[1] == "removed":
									S += f" Reference-line removed ({frm[2]})"
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
								S += f"\n\t{frm[1]!r}\n\tdiffers from: {frm[2]!r}"
							elif frm[1] == "Leading white-spaces":
								at_pos = [str(n) for n in frm[0]]
								if frm[2] in ("start","end"):
									S += f"\n\t@ {':'.join(at_pos)}: Leading white-spaces at {frm[2]}:"
								else:
									into = [str(n) for n in frm[2]]
									S += f"\n\t@ {':'.join(at_pos)}: Leading white-spaces at {':'.join(into)}:"
								S += f"\n\t\t{frm[3]} differs from: {frm[4]}"
							else:# frm[1] == "Formating":
								at_pos = [str(n) for n in frm[0]]
								S += f"\n\t@ {':'.join(at_pos)}: Formating:"
								if not frm[3] is None:
									S += f"\n\t\t{frm[2]} differs from: {frm[3]}"
								else: S += f"\n\t\t{frm[2]} is missing in this translation"
		Verb(S)
		fPath = os.path.split(forF)
		fName = os.path.splitext(fPath[1])[0]+'.diff'
		with open(os.path.join(fPath[0],fName), 'w') as f:
			f.write(S)
diff.__doc__ = _("""\
forFiles: List - The list of files where differences of translation need to be compared.
FromFiles: List,... - Successions of list of files from where to get other translations.
verbose: Integer - Indicate the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
""")

def check(forFiles, /, untranslated=1, formats=False, *, verbose=0, debug=False):
	Verb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=1 else None
	xVerb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=2 else None
	Debug = lambda *args, **kwargs: print(*args, **kwargs) if debug else None
	if not untranslated in (0,1,2):
		Throw(ValueError,_("Invalide argument:"),_("untranslated parameter should be 1 for empty, 2 for pass, or 0 to deactivate"), code=2)
	if not isinstance(formats, bool):
		Throw(TypeError,_("Invalide argument:"),_("formats parameter should be of booleen type"), code=2)
	if not formats and untranslated == 0:
		Throw(Exception,_("Invalide parameters:"),_("formats parameter need to be set to True or untranslated parameter need to be greater than 0 to indicate which checking to make"), code=3)
	for i,forF in enumerate(forFiles):
		if not os.path.isfile(forF):
			Throw(FileNotFoundError,repr(forF),_("doen't exist or is not a file"))
		if forF in forFiles[:i]: Throw(Exception,_("All files need to be different"))

	RE_string = re.compile(reString.format(old=r'old +(?P<old_str>{Q})'.format(Q=reDQ), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
	RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{Q})'.format(N=reN, Q=reDQ), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
	file_cache, frm_res = {}, {}
	for forF in forFiles:
		Verb('forFile:',repr(forF))
		if not forF in file_cache.keys():
			F = ""
			with open(forF, 'r', newline='') as f:
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
					M = M_dialog
				else:
					M = M_string
			else: M = M_string if M_dialog is None else M_dialog
			Pass = not RE_pass.match(M.group('new')) is None
			Debug(f'Debug:match span {M.span()} is:\n{M.group()!r}')
			Debug("Debug: new_str group =",repr(M.group('new_str')) if not Pass else 'pass')
			if Pass or M.group('new_str') == '""':
				if untranslated == 2:
					ppl += 1
					xVerb(f"For: {M.group('old')}\nTranslation is: {'pass' if Pass else M.group('new')}")
				elif not Pass and untranslated == 1:
					ppl += 1
					xVerb(f"For: {M.group('old')}\nTranslation is: {M.group('new')}")
			elif formats:
				res = []
				o_str,n_str = eval(M.group('old_str')),eval(M.group('new_str'))# since we now they are string, np
				o,n = re.search(r'^\s*', o_str),re.search(r'^\s*', n_str)
				if o.group() != n.group():
					res += [(o.span(),"Leading white-spaces","start",repr(o.group()),repr(n.group()))]
				o,n = re.search(r'\s*$', o_str),re.search(r'\s*$', n_str)
				if o.group() != n.group():
					res += [(o.span(),"Leading white-spaces","end",repr(o.group()),repr(n.group()))]
				rx=r'({{(\\}}|}?[^}])*}})|({(\\}|[^}])*})|(\[(\\\]|[^]])*\])'
				RE_x = re.compile(r'\s*{x}\s*'.format(x=rx))
				o,n, n_end = RE_x.search(o_str),RE_x.search(n_str), 0
				while not o is None:
					if n is None:
						res += [(o.span(),"Formating",repr(o.group()),None)]
					elif o.group() != n.group():
						n_end = n.end()
						_o,_n = re.search(rx, o.group()),re.search(rx, n.group())
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

	if len(frm_res.keys()) > 0:
		for forF, frmF in frm_res.items():
			S, keys = f"For: {forF}", sorted(frmF.keys())
			if untranslated != 0:
				S += "\nNot translated: "+str(file_cache[forF+':E'])
			for k in keys:
				at = [str(Line(file_cache[forF+':L'], n)) for n in k]
				S += f"\n@Lines {':'.join(at)}:"
				for frm in frmF[k]:
					at_pos = [str(n) for n in frm[0]]
					if frm[1] == "Leading white-spaces":
						if frm[2] in ("start","end"):
							S += f"\n\t@ {':'.join(at_pos)}: Leading white-spaces at {frm[2]}:"
						else:
							into = [str(n) for n in frm[2]]
							S += f"\n\t@ {':'.join(at_pos)}: Leading white-spaces at {':'.join(into)}:"
						S += f"\n\t\t{frm[3]} differs in the translation: {frm[4]}"
					else:
						S += f"\n\t@ {':'.join(at_pos)}: Formating:"
						if not frm[3] is None:
							S += f"\n\t\t{frm[2]} differs in the translation: {frm[3]}"
						else: S += f"\n\t\t{frm[2]} is missing in the translation"
			xVerb(S)
			fPath = os.path.split(forF)
			fName = os.path.splitext(fPath[1])[0]+'.check-info'
			with open(os.path.join(fPath[0],fName), 'w') as f:
				f.write(S)
	elif untranslated != 0:
		for k in [f for f in file_cache.keys() if f.endswith(':E')]:
			fPath = os.path.split(k[:-2])
			fName = os.path.splitext(fPath[1])[0]+'.check-info'
			with open(os.path.join(fPath[0],fName), 'w') as f:
				f.write("Not translated: "+str(file_cache[k]))
check.__doc__ = _("""\
forFiles: List - The list of files where translations need to be checked.
untranslated: Integer - Should be: 1 to check empty, 2 to also check passed, or 0 to deactivate.
formats: Booleen - Check or no the formating into translations.
         Like leading white-spaces, brackets ([]) and braces ({}).
verbose: Integer - Indicate the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
""")

def fixEmpty(forFiles, /, action='P', *, outdir=None, verbose=0, debug=False):
	Verb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=1 else None
	xVerb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=2 else None
	Debug = lambda *args, **kwargs: print(*args, **kwargs) if debug else None
	if not action in ('P','C','R'):
		Throw(ValueError,_("Invalide argument:"),_("action parameter should be P, C, or R"), code=2)
	popFiles = []
	for forF in forFiles:
		if not os.path.isfile(forF):
			Throw(FileNotFoundError,repr(forF),_("doen't exist or is not a file"))
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
	RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{Q})'.format(N=reN, Q=reDQ), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
	glob_ppl = 0
	file_cache = {}
	for forF in forFiles:
		Verb('forFile:',repr(forF))
		if not forF in file_cache.keys():
			F, dbg = "", 0
			with open(forF, 'r', newline='') as f:
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
					if not Pass and T == 0:
						file_cache[forF] = file_cache[forF][:M.start()]+file_cache[forF][M.end():]
						pos = M.start()+1
						ppl += 1
					elif not Pass and T == 1:
						n_str = M.group().replace(M.group('new'), 'pass')
						file_cache[forF] = file_cache[forF][:M.start()]+n_str+file_cache[forF][M.end():]
						pos = M.start()+len(n_str)
						ppl += 1
				elif action == 'C':
					if not Pass and T == 0:
						o_str, n_str, ind = M.group('old'), M.group('new'), M.group('ind')
						o_str = re.sub(r'^({I})?'.format(I=ind), r'\1#', o_str)
						n_str = re.sub(r'^({I})?'.format(I=ind), r'\1#', n_str)
						n_str = M.group().replace(M.group('old'), o_str).replace(M.group('new'), n_str)
						file_cache[forF] = file_cache[forF][:M.start()]+n_str+file_cache[forF][M.end():]
						pos = M.start()+len(n_str)
						ppl += 1
					elif not Pass and T == 1:
						n_str = M.group().replace(M.group('new'), 'pass')
						file_cache[forF] = file_cache[forF][:M.start()]+n_str+file_cache[forF][M.end():]
						pos = M.start()+len(n_str)
						ppl += 1
				elif action == 'R':
					file_cache[forF] = file_cache[forF][:M.start()]+file_cache[forF][M.end():]
					pos = M.start()+1
					ppl += 1
			else: pos = M.end()
			M_string, M_dialog = RE_string.search(file_cache[forF], pos), RE_dialog.search(file_cache[forF], pos)
		Verb('A total of',ppl,'empty translations was fixes')
		glob_ppl += ppl
	Verb('A total of',glob_ppl,'empty translations was fixes in globality')

	for forF, popF in zip(forFiles, popFiles):
		with open(popF, 'w', newline='') as f:
			f.write(file_cache[forF])
fixEmpty.__doc__ = _("""\
forFiles: List - The list of files where empty translations need to be fixed.
action: Char - Should be: 'P' to change empty translation to the 'pass' keyword, 'C' to also comment
        'strings' translations instead of deleting them, or 'R' to remove all empty or passed translation.
outdir: String - A path relative from the 'forFiles' paths to indicate where to save the result or None
        to save inplace.
verbose: Integer - Indicate the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
""")

def reorder(forFiles, /,*, reverse=False, proxy=False, outdir=None, verbose=0, debug=False):
	Verb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=1 else None
	xVerb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=2 else None
	Debug = lambda *args, **kwargs: print(*args, **kwargs) if debug else None
	if not isinstance(reverse, bool):
		Throw(TypeError,_("Invalide argument:"),_("reverse parameter should be of booleen type"), code=2)
	if not isinstance(proxy, bool):
		Throw(TypeError,_("Invalide argument:"),_("proxy parameter should be of booleen type"), code=2)
	popFiles = []
	for forF in forFiles:
		if not os.path.isfile(forF):
			Throw(FileNotFoundError,repr(forF),_("doen't exist or is not a file"))
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

	RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{Q})'.format(N=reN, Q=reDQ), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
	glob_ppl = 0
	file_cache = {}
	for forF in forFiles:
		Verb('forFile:',repr(forF))
		if not forF in file_cache.keys():
			F, dbg = "", 0
			with open(forF, 'r', newline='') as f:
				r=None
				while r!="":
					r = f.read()
					dbg += 1
					Debug('Debug:read pass',dbg)
					F += r
			file_cache[forF] = F

		buffer = {}
		start,end, pos, ppl = -1,-1, 0, 0
		M, M_dialog = None, RE_dialog.search(file_cache[forF], 0)
		while not M_dialog is None:
			M = M_dialog
			if reverse:
				tr_id = (M.group('file'),int(M.group('line')),M.group('start')) if not M.group('file') is None else None
			else:
				tr_id = N_str(M.group('old_str')) if proxy else M.group('old_str')
			if not tr_id is None:
				if start == -1: start = M.start()
				pos, end = M.start(), M.end()
				if not tr_id in buffer.keys():
					buffer[tr_id] = []
					pos = end
				else:
					file_cache[forF] = file_cache[forF][:M.start()]+file_cache[forF][end:]
				if reverse: file_cache[forF] = file_cache[forF][:M.start()]+file_cache[forF][end:]
				buffer[tr_id].append(M.group())
			else: pos = M.end()
			M_dialog = RE_dialog.search(file_cache[forF], pos)

		if reverse:
			xpos = start
			for tr_key in sorted(buffer.keys()):
				file_cache[forF] = file_cache[forF][:xpos]+buffer[tr_key][0][xpos:]
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
		with open(popF, 'w', newline='') as f:
			f.write(file_cache[forF])
reorder.__doc__ = _("""\
forFiles: List - The list of files where translations need to be reordered.
reverse: Booleen - Reordering as it was originaly (as extracted by Ren'Py) or process normaly.
proxy: Booleen - Active or not the regroupement in function of their normalized alpha-numeric
       representation.
outdir: String - A path relative from the 'forFiles' paths to indicate where to save the result or None
        to save inplace.
verbose: Integer - Indicate the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging.
""")

def populate(forFiles, *FromFiles, proxy=0, overwrite='N', outdir=None, verbose=0, debug=False):
	Verb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=1 else None
	xVerb = lambda *args, **kwargs: print(*args, **kwargs) if verbose>=2 else None
	Debug = lambda *args, **kwargs: print(*args, **kwargs) if debug else None
	if not proxy in (0,1,2,3):
		Throw(ValueError,_("Invalide argument:"),_("proxy should be an integer between 0 and 3 (includes)"), code=2)
	if not overwrite in ('N','A','F'):
		Throw(ValueError,_("Invalide argument:"),_("overwrite parameter should be N for no, A for ask, or F for force"), code=2)
	popFiles = []
	for fromFiles in FromFiles:
		if len(forFiles) != len(fromFiles):
			Throw(Exception,_("The length of both list of files need to be equal"))
		for forF, fromF in zip(forFiles, fromFiles):
			if not os.path.isfile(forF):
				Throw(FileNotFoundError,repr(forF),_("doen't exist or is not a file"))
			if not os.path.isfile(fromF):
				Throw(FileNotFoundError,repr(fromF),_("doen't exist or is not a file"))
			if forF == fromF:
				Throw(Exception,_("Both list of files need to have their files different"))
			if not outdir is None:
				fPath = os.path.split(forF)
				if not os.path.isdir(os.path.join(fPath[0],outdir)):
					os.mkdir(os.path.join(fPath[0],outdir))
				popFiles.append(os.path.join(fPath[0],outdir,fPath[1]))
			else: popFiles.append(forF)

	RE_string = re.compile(reString.format(old=r'old +(?P<old_str>{Q})'.format(Q=reDQ), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
	RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{Q})'.format(N=reN, Q=reDQ), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
	glob_ppl = 0
	file_cache = {}
	def _search(what, fromF):
		M, M_string,M_dialog = None, RE_string.search(file_cache[fromF],pos), RE_dialog.search(file_cache[fromF],pos)
		corresp, w = {}, eval(what)# since we now this is string, np
		while not (M_string is None and M_dialog is None):
			if not M_string is None and not M_dialog is None:
				if M_dialog.start() < M_string.start():
					M = M_dialog
				else:
					M = M_string
			else:
				M = M_string if M_dialog is None else M_dialog
			Pass = not RE_pass.match(M.group('new')) is None
			if not Pass and M.group('new_str') != '""':
				c = cmp(w, eval(M.group('old_str')))# since we now this is string, np
				if c >= .90: corresp[(c, M.start())] = M
			pos = M.end()
			M_string,M_dialog = RE_string.search(file_cache[fromF],pos), RE_dialog.search(file_cache[fromF],pos)
		return corresp[sorted(corresp.keys())[-1]] if len(corresp.keys()) != 0 else None
	def _populate(forFiles, fromFiles):
		nonlocal glob_ppl
		group_ppl = 0
		for forF, fromF in zip(forFiles, fromFiles):
			Verb('forFile:',repr(forF))
			if not forF in file_cache.keys():
				F, dbg = "", 0
				with open(forF, 'r', newline='') as f:
					r=None
					while r!="":
						r = f.read()
						dbg += 1
						Debug('Debug:read pass',dbg)
						F += r
				file_cache[forF] = F
			Verb('fromFile:',repr(fromF))
			if not fromF in file_cache.keys():
				F, dbg = "", 0
				with open(fromF, 'r', newline='') as f:
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
					_RE_dialog = re.compile(reDialog.format(old=re.escape(M.group('old')), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
					_RE_string = re.compile(reString.format(old=re.escape(M.group('old')), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
					if T == 1:
						RE_from = _RE_dialog
					else:
						RE_from = _RE_string
					m_from = RE_from.search(file_cache[fromF])
					while not m_from is None:# get the eventual latest
						if not Pass: M_from = m_from
						m_from = RE_from.search(file_cache[fromF], m_from.end())
					if M_from is None:
						if proxy > 0:
							by_proxy = True
							_RE_dialog = re.compile(reDialog.format(old=r'# ({N} +)?(?P<old_str>{old})'.format(N=reN, old=re.escape(M.group('old_str'))), new=r'({Pass}|({N} +)?(?P<new_str>{P}))'.format(N=reN, P=reP, Pass=rePass), py=rePy, rID=reRID), re.M|re.S)
							_RE_string = re.compile(reString.format(old=r'old +(?P<old_str>{old})'.format(old=re.escape(M.group('old_str'))), new=r'new +(?P<new_str>{P})'.format(P=reP), rID=reRID), re.M|re.S)
							if proxy >= 1:
								if T == 1:
									m_from = _RE_string.search(file_cache[fromF])
									while not m_from is None:# get the eventual latest
										if not Pass: M_from = m_from
										m_from = _RE_string.search(file_cache[fromF], m_from.end())
							if M_from is None and proxy >= 2:
								if T == 0:
									m_from = _RE_dialog.search(file_cache[fromF])
									while not m_from is None:# get the eventual latest
										if not Pass: M_from = m_from
										m_from = _RE_dialog.search(file_cache[fromF], m_from.end())
								else:
									pass# same as for proxy 1
							if M_from is None and proxy >= 3:
								M_from = _search(M.group('old_str'), fromF)
						if M_from is None: Debug('Debug:from_match span (-1, -1) is:\n""')
					else: Debug(f'Debug:from_match span {M_from.span()} is:\n{M_from.group()!r}')
					if not M_from is None and M_from.group('new_str') == '""': M_from = None
				if not M_from is None:
					A = None
					if (not by_proxy and not Pass and not M.group('new_str') in ('""', M_from.group('new_str')) and overwrite == 'A') or (by_proxy and (Pass or M.group('new_str') == '""')):
						print(_("For: {old}\nAttempt to replace: {new}\nFrom: {From}").format(old=M.group('old_str'), new=M.group('new_str') if not Pass else 'pass', From=M_from.group('new_str')))
						if by_proxy: print(_("Warning:"),_("Please note that the 'From' was found by proxy."))
						while not A:
							A = input(_("Proceed?")+" (N|Y) >>> ")
							if A.upper() in ('Y','YES'): A='P' if by_proxy else 'Y'
							elif A.upper() in ('N','NO'): A='N'
							else: A=None
					if ((not by_proxy or A == 'P') and (Pass or M.group('new_str') == '""')) or (not by_proxy and (M.group('new_str') != M_from.group('new_str') and (overwrite == 'F' or A == 'Y'))):
						if not Pass:
							xF = file_cache[forF][M.start():].replace(M.group('new_str'), M_from.group('new_str'), 1)
						else:
							xF = file_cache[forF][M.start():].replace(M.group('new'), M.group('old').replace(M.group('old_str'), M_from.group('new_str'), 1), 1)
						xVerb('Change translation from',repr(M.group('new_str') if not Pass else M.group('old_str')),'\tto',repr(M_from.group('new_str')))
						file_cache[forF] = file_cache[forF][:M.start()]+xF
						ppl += 1
					else: M_from = None
				pos = M.end()-(len(M.group('new'))-len(M_from.group('new')) if not M_from is None else 0)
				M_string, M_dialog = RE_string.search(file_cache[forF], pos), RE_dialog.search(file_cache[forF], pos)
			Verb('A total of',ppl,'translations was populate')
			group_ppl += ppl
			glob_ppl += ppl
		Verb('A total of',group_ppl,'translations was populate with this group of from-files')

		for forF, popF in zip(forFiles, popFiles):
			with open(popF, 'w', newline='') as f:
				f.write(file_cache[forF])

	_populate(forFiles, FromFiles[0])
	if len(FromFiles) > 1:
		for fromFiles in FromFiles[1:]:
			_populate(popFiles, fromFiles)
	Verb('A total of',glob_ppl,'translations was populate in globality')
populate.__doc__ = _("""\
forFiles: List - The list of files that need to be populated.
FromFiles: List,... - Successions of list of files from where to get the translations.
proxy: Integer - From 0 to 3 (includes) to indicate a level of proxy, 0 to diactivate, 3 made it long.
overwrite: Char - Should be: 'N' for no, 'A' for ask, or 'F' for force.
outdir: String - A path relative from the 'forFiles' paths to indicate where to save the result or None
        to save inplace.
verbose: Integer - Indicate the level of verbosity, 0 to deactivate.
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
		  Please note that help commands obviousely doesn't accept any args or options even if they still
		   show the requested help.
		args
		  Their are specific for each command and generaly should be put in a specific order.
		  See the specific help of these commands for more information.
		options
		  They generaly can be put everywhere in the argument realm of command.
		  Though commands has their own set of options, here are some general options below:
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
		  -o dir, --subdir dir
		      If put, it indicate a sub-directory to where the populated translation files are saved,
		       otherwise on the for-files themself.
		  -% n, --multi n, --lists n
		      This allow to give the number of list from where merging multiple translations. This include
		       the first group of files (the for_files).
		      So, for 2 from_files, you should put it with 3.
		      The internal workflow is like:
		          for_files - from_files[0] > pop_files ; pop_files - from_files[1] > pop_files ; ...
		          * Where from_files[#] are group of files of the same length as forFiles.
		  -a, --ask
		      If put, a prompt will ask to confirm or no a replacement of translation (only for non-empty).
		  -f, --force, --overwrite
		      Activate the replacement of existing translations.
		  --proxy lvl
		      If a translation is not found with the normals methods, so this option allow to use other methods
		       based on the proxymity level (lvl).
		      lvl can be one of the following:
		          0   Default. No proxy maked.
		          1   For 'dialogs', allow to search in 'strings' translations, but not allow for 'strings'
		               to search in 'dialogs' translations.
		          2   Allow to focus only on the strings. With that, 'dialogs' can now be considered as same
		               as 'strings', so 'strings' can now search in 'dialogs' translations.
		          3   The strings are compared to get a least a correspondance of 90%.
		              Please note that this can be a very long process.
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process. The output can be huge.
		* Please note that --ask take precedence over --force.
		""")))
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
		      If put, it indicate a sub-directory to where the fixed translation files are saved, otherwise
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
		  -f, --format
		      Sometimes leading white-spaces can be typos but generaly they are on purpose (especialy when an
		       'extend' is in use).
		      Check if the translations had keep all kinds of formating and that leading white-spaces are
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
		  -% n, --multi n, --lists n
		      This allow to give the number of list from where other translations are get. This include the
		       first group of files (the for_files).
		      So, for 2 from_files, you should put it with 3.
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
	elif Cmd == 'reorder' and sys.argv[1] in ('-h','--help'):
		print(dedent(_("""\
		rpyTranslations reorder [--help] for_files... [options]

		for_files are the reference files where the translations need to be reordered.

		This command try regrouping similiar translations to ease translations and comprisons of equivalant
		 translations.
		Since 'strings' translations are generaly unique, it should be note that it's only work for 'dialogs'.
		Also, all occurences are regrouping to the first occurence place.

		options:
		  -h, --help
		      Show this specific help for the 'reorder' command and exit.
		  -o dir, --subdir dir
		      If put, it indicate a sub-directory to where the reordered translation files are saved,
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
	args,kargs = [],{}
	if '-v' in sys.argv:
		verbose = 1
		try:
			verbose = int(sys.argv[sys.argv.index('-v')+1])
			sys.argv.pop(sys.argv.index('-v')+1)
		except: pass
		sys.argv.remove('-v')
	elif '--verbose' in sys.argv:
		verbose = 1
		try:
			verbose = int(sys.argv[sys.argv.index('--verbose')+1])
			sys.argv.pop(sys.argv.index('--verbose')+1)
		except: pass
		sys.argv.remove('--verbose')
	if '-d' in sys.argv:
		dbg = True
		sys.argv.remove('-d')
	elif '--debug' in sys.argv:
		dbg = True
		sys.argv.remove('--debug')
	if dbg: print("Debug: args =",sys.argv)
	if Cmd == 'populate':
		O, D, N = 'N', None, 2
		if '-f' in sys.argv:
			O = 'F'
			sys.argv.remove('-f')
		elif '--force' in sys.argv:
			O = 'F'
			sys.argv.remove('--force')
		elif '--overwrite' in sys.argv:
			O = 'F'
			sys.argv.remove('--overwrite')
		if '-a' in sys.argv:
			O = 'A'
			sys.argv.remove('-a')
		elif '--ask' in sys.argv:
			O = 'A'
			sys.argv.remove('--ask')
		if '-o' in sys.argv:
			D = sys.argv.pop(sys.argv.index('-o')+1)
			sys.argv.remove('-o')
		elif '--subdir' in sys.argv:
			D = sys.argv.pop(sys.argv.index('--subdir')+1)
			sys.argv.remove('--subdir')
		if '-%' in sys.argv:
			N = sys.argv.pop(sys.argv.index('-%')+1)
			try: N = int(N)
			except: Error(("Invalide argument: -% option should be an integer, give"),N)
			sys.argv.remove('-%')
		elif '--multi' in sys.argv:
			N = sys.argv.pop(sys.argv.index('--multi')+1)
			try: N = int(N)
			except: Error(("Invalide argument: --multi option should be an integer, give"),N)
			sys.argv.remove('--multi')
		elif '--lists' in sys.argv:
			N = sys.argv.pop(sys.argv.index('--lists')+1)
			try: N = int(N)
			except: Error(("Invalide argument: --lists option should be an integer, give"),N)
			sys.argv.remove('--lists')
		if '--proxy' in sys.argv:
			proxy = sys.argv.pop(sys.argv.index('--proxy')+1)
			try: proxy = int(proxy)
			except: Error(("Invalide argument: --proxy option should be an integer, give"),proxy)
			sys.argv.remove('--proxy')
		else: proxy = 0
		argc = len(sys.argv)-1
		if argc%N != 0:
			Error(_("The file pair lists need to have equal length, {N} gived.", argc).format(N=argc))
		N = argc//N
		files1 = sys.argv[1:N+1]
		files2 = []
		for i,f in enumerate(sys.argv[1+N:]):
			if i%N == 0: files2.append([f])
			else: files2[-1].append(f)
		if dbg: print(f"Debug: populate({files1}, {files2}, proxy={proxy}, overwrite={O!r}, outdir={D!r}, verbose={verbose}, debug={dbg})")
		args,kargs = [files1,*files2], {'proxy':proxy,'overwrite':O,'outdir':D,'verbose':verbose,'debug':dbg}
	elif Cmd == 'fix-empty':
		A, D = 'P', None
		if '-a' in sys.argv:
			A = sys.argv.pop(sys.argv.index('-a')+1)
			sys.argv.remove('-a')
		elif '--action' in sys.argv:
			A = sys.argv.pop(sys.argv.index('--action')+1)
			sys.argv.remove('--action')
		if '-o' in sys.argv:
			D = sys.argv.pop(sys.argv.index('-o')+1)
			sys.argv.remove('-o')
		elif '--subdir' in sys.argv:
			D = sys.argv.pop(sys.argv.index('--subdir')+1)
			sys.argv.remove('--subdir')
		files = sys.argv[1:]
		if dbg: print(f"Debug: fixEmpty({files}, action={A!r}, outdir={D!r}, verbose={verbose}, debug={dbg})")
		args,kargs = [files], {'action':A,'outdir':D,'verbose':verbose,'debug':dbg}
	elif Cmd == 'check':
		UnT, F = 1, False
		if '-e' in sys.argv:
			UnT = 1
			sys.argv.remove('-e')
		elif '--empty' in sys.argv:
			UnT = 1
			sys.argv.remove('--empty')
		if '-p' in sys.argv:
			UnT = 2
			sys.argv.remove('-p')
		elif '--not-translate' in sys.argv:
			UnT = 2
			sys.argv.remove('--not-translate')
		if '-n' in sys.argv:
			UnT = 0
			sys.argv.remove('-n')
		elif '--skip-empty' in sys.argv:
			UnT = 0
			sys.argv.remove('--skip-empty')
		if '-f' in sys.argv:
			F = True
			sys.argv.remove('-f')
		elif '--format' in sys.argv:
			F = True
			sys.argv.remove('--format')
		files = sys.argv[1:]
		if dbg: print(f"Debug: check({files}, untranslated={UnT}, formats={F!r}, verbose={verbose}, debug={dbg})")
		args,kargs = [files], {'untranslated':UnT,'formats':F,'verbose':verbose,'debug':dbg}
	elif Cmd == 'diff':
		N = 2
		if '-%' in sys.argv:
			N = sys.argv.pop(sys.argv.index('-%')+1)
			try: N = int(N)
			except: Error(_("Invalide argument:"),_("-% option should be an integer, give"),N)
			sys.argv.remove('-%')
		elif '--multi' in sys.argv:
			N = sys.argv.pop(sys.argv.index('--multi')+1)
			try: N = int(N)
			except: Error(_("Invalide argument:"),_("--multi option should be an integer, give"),N)
			sys.argv.remove('--multi')
		elif '--lists' in sys.argv:
			N = sys.argv.pop(sys.argv.index('--lists')+1)
			try: N = int(N)
			except: Error(_("Invalide argument:"),_("--lists option should be an integer, give"),N)
			sys.argv.remove('--lists')
		argc = len(sys.argv)-1
		if argc%N != 0:
			Error(_("The file pair lists need to have equal length, {N} gived.", argc).format(N=argc))
		N = argc//N
		files1 = sys.argv[1:N+1]
		files2 = []
		for i,f in enumerate(sys.argv[1+N:]):
			if i%N == 0: files2.append([f])
			else: files2[-1].append(f)
		if dbg: print(f"Debug: diff({files1}, {files2}, verbose={verbose}, debug={dbg})")
		args,kargs = [files1,*files2], {'verbose':verbose,'debug':dbg}
	elif Cmd == 'reorder':
		D, R, P = None, False, False
		if '-o' in sys.argv:
			D = sys.argv.pop(sys.argv.index('-o')+1)
			sys.argv.remove('-o')
		elif '--subdir' in sys.argv:
			D = sys.argv.pop(sys.argv.index('--subdir')+1)
			sys.argv.remove('--subdir')
		if '-r' in sys.argv:
			R = True
			sys.argv.remove('-r')
		elif '--reverse' in sys.argv:
			R = True
			sys.argv.remove('--reverse')
		elif '--proxy' in sys.argv:
			P = True
			sys.argv.remove('--proxy')
		files = sys.argv[1:]
		if dbg: print(f"Debug: check({files}, reverse={R!r}, proxy={P!r}, outdir={D!r}, verbose={verbose}, debug={dbg})")
		args,kargs = [files], {'outdir':D,'reverse':R,'proxy':P,'verbose':verbose,'debug':dbg}
	#print("Debug: STOP")
	#exit()
	try:
		if Cmd == 'popuplate':
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
