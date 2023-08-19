//----------------------------------------------------------------------------------------
//-- PREPROCESSOR --------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------
// work in progress, full of flaws..
#define DEBUGDEFINE //word += toString(inIf)

#if DEFINED_INCLUDE_REDUCED_SHADER_PREPROCESSOR == 1

using DEFINE = std::pair<std::pair<std::string, std::string>, std::string>;
enum {
    TYPETOLOOKFOR_NONE = 0,
    TYPETOLOOKFOR_DEFINEHEADER_START,
    TYPETOLOOKFOR_DEFINEHEADER_BASE,
    TYPETOLOOKFOR_DEFINEHEADER_PARAMETERS,
    TYPETOLOOKFOR_DEFINE_CONTENT_START,
    TYPETOLOOKFOR_DEFINE_CONTENT,
    TYPETOLOOKFOR_DEFINELINE_DONE,
};
#define REDOCHAR 0x01
#define FINISHCHAR LFCHAR
#define TRIGGERWORDRESOLVECHAR LFCHAR
#define REMOVE_EMPTY_COMMENTS (1<<0)
#define REMOVE_MULTILINE_MULTI_COMMENTS (1<<1)
#define ERASE_COMMENTS (1<<2)
#define CONVERT_TO_SINGLE_LINE_COMMENTS (1<<3)
#define ERASE_PREPROCESSOR_DIRECTIVES (1<<4)
#define __PHASE(...) (std::string(__VA_ARGS__) + (char)LFCHAR)
#define ISPHASEDEFINE(__a,__phaseId) (((__a).first.first.length() >= (__phaseId).length()) && ((__a).first.first.substr(0,(__phaseId).length()) == (__phaseId)))
#define PHASEDEFINE(__a,__phaseId) ((__phaseId) + (__a))
#define FINALPHASE __PHASE("__FINAL_PHASE__")
#define BTRUE "true"
#define BFALSE "false"
#define dsprintf //printf
#define MAXRESOLVEITERATIONS 10 // for debugging purposes

std::string withoutExoticStuff(const std::string& c);
std::string exoticStuffAgain(const std::string& c);
void stringToFile(const std::string& data, const std::string& outputFileName);
const std::string resolve(const std::string& word, const std::vector<DEFINE>& defines);
std::string trim(const std::string& s) { int i = 0; for (i = 0; i < s.length(); ++i) if (s[i] != ' ' && s[i] != '\t' && s[i] != LFCHAR) break; int start = i; for (i = s.length() - 1; i >= start; --i) if (s[i] != ' ' && s[i] != '\t' && s[i] != LFCHAR) break; return s.substr(start, i - start + 1); }
std::string evaluateSimpleBooleanExpression(const std::string& s, const std::vector<DEFINE>& defines, const std::string& configuration = "");
const std::string getDefineString(const DEFINE& define) { const bool h = !define.first.second.empty(); return exoticStuffAgain(define.first.first + (h ? (";" + define.first.second) : "") + ";" + define.second); }
std::string allDefinesAsString(const std::vector<DEFINE>& defines) {std::string r; for (int i = 0; i < defines.size(); ++i) {r += "//;------------------==>>>;\n"; r += std::string("#define ") + getDefineString(defines[i]) + "\n";} return r; }
std::vector<DEFINE> phaseDefines(const std::vector<DEFINE>& defines, const std::string &phaseId);

#define DEFINED_MAC_PREPROCESSOR_DEFINES 1
#define DEFINED_MAC_PREPROCESSOR_IFS 1

#define GETDATA(__pos) (((__pos) < fileEnd) ? data[__pos] : FINISHCHAR)
#define SETDATA(__pos,__c) {if ((__pos) < fileEnd) {data[__pos] = (__c);}}
#define CLEARADDWORDS if (onlySelectedSections) {addWords = false;}
#define INVERTADDWORDS if (onlySelectedSections) {if (ADDWORDSINSECTION) addWords = false;} // ifdefs anyways here
#define ADDWORD(__a) if (addWords) {nextPass += (__a);}
#define READPOSUPDATE {if (c != REDOCHAR) {lc = c; readPos++;} else {ADDWORD(word);word = "";}}
#define ADDCHAR(__s, __c) {if (__c != REDOCHAR) (__s) += (__c);}
#define CNC(__a) (c == (__a)[0] && nc == (__a)[1])
#define RP1(__a) (GETDATA(readPos) == (__a)[0] && GETDATA(readPos+1) == (__a)[1])
#define ADDADDITIONALLINECOMMENT { if (!additionalLineComment.empty()) { nextPass += additionalLineComment; additionalLineComment = ""; } }
#define ADDADDITIONALCOMMENT { if (!additionalComment.empty()) { nextPass += additionalComment; additionalComment = ""; } if (c == LFCHAR) ADDADDITIONALLINECOMMENT; }
#define ACTIVATIONINDENTLEVEL (commentedOutIndents.size() - 1)
#define COMMENTEDOUTSECTION ((commentedOutIndents[ACTIVATIONINDENTLEVEL] & 1) != 0)
#define ADDWORDSINSECTION ((commentedOutIndents[ACTIVATIONINDENTLEVEL] & 2) == 0)
#define SETADDWORDS {addWords=true; commentedOutIndents[ACTIVATIONINDENTLEVEL] |= 2;}
#define COMMENTINSECTION {commentedOutIndents[ACTIVATIONINDENTLEVEL] &= ~1;}
#define COMMENTOUTSECTION {commentedOutIndents[ACTIVATIONINDENTLEVEL] = 1;}
#define INVERTSECTION {commentedOutIndents[ACTIVATIONINDENTLEVEL] ^= 1;}
#define DEFINEINDENT {commentedOutIndents.push_back(0);}
#define DEFINEUNINDENT {commentedOutIndents.resize(commentedOutIndents.size()-1); addWords = ADDWORDSINSECTION;}
#if DEFINED_REDUCED_SHADER_PREPROCESSOR_WITH_SIMPLE_BOOLEAN_EXPRESSIONS == 1
#define EVALUATEEXPRESSION(__e) (((word = evaluateSimpleBooleanExpression((__e), defines, configuration)) != "") && (((!finalPhase) && (__e) == BTRUE) || (finalPhase && (__e) == configuration)))
#else
#define EVALUATEEXPRESSION(__e) true
#endif
#define OVERJUMPSLCOMMENT if (CNC("//")) { if (!word.empty() && !inPreprocessor) {ADDCHAR(word, TRIGGERWORDRESOLVECHAR); c = REDOCHAR; } else { ADDWORD(word); word = ""; for (; readPos < fileEnd; ++readPos) { char c = GETDATA(readPos); if (c == LFCHAR) { break; } ADDCHAR(word,c); ADDWORD(word); word = "";} lc = c; continue; } } // algo (thispass and nextpass) has to get new line after the comment aswell
#define OVERJUMPMLCOMMENT if (CNC("/*")) { ADDWORD("/");readPos++; int ind = 1; for (; readPos < fileEnd; ++readPos) { char c = GETDATA(readPos); char nc = GETDATA(readPos+1); if (RP1("*/")) { ind--; if (ind == 0) { break; } if (CNC("//")||CNC("*/")||CNC("*/")) {SETDATA(readPos,'|');SETDATA(readPos+1,'|');readPos++;} } if (RP1("/*")) {ind++;} ADDCHAR(word, GETDATA(readPos)); } ADDWORD(word+"*/"); word = ""; readPos += 2;  lc = c; continue; }
#define MAYBEEVALUATEIFSECTION {\
if (ifDefineSection) {\
    OVERJUMPMLCOMMENT \
    printf("%c",c);                                \
    if ((c == LFCHAR && lc != '\\') || c == '#' || CNC("//")) { ifDefineSection = false;\
        if (pass == 0) {\
            ifSectionDescription = "["+word+"]"; commentForIndent[ACTIVATIONINDENTLEVEL] = ifSectionDescription;\
        } else {ifSectionDescription == "";}\
        word = trim(word);\
        if(!EVALUATEEXPRESSION(word)) {COMMENTOUTSECTION; ADDWORD(" "); ADDWORD(word); ADDWORD("/* "); ADDWORD(ifSectionDescription); ADDWORD(" ");} else {ADDWORD(" "); ADDWORD(word); }\
        word = "";\
        if (ifDef && onlySelectedSections) {\
            if (COMMENTEDOUTSECTION) {SETADDWORDS; ADDWORD("// ------------ CHOOSEN SECTION ------------\n// ----------------------- \n"); COMMENTINSECTION;}\
        }                       \
        OVERJUMPSLCOMMENT;      \
        ifDef = false;          \
    } else { ADDCHAR(word,c); READPOSUPDATE; continue; }\
}}

std::string preprocessString(const std::string& inputString, const std::vector<DEFINE>& sDefines = std::vector<DEFINE>()) {
    std::string data = inputString;
    std::string lastPass = data;
    const int passes = DEFINED_REDUCED_SHADER_PREPROCESSOR_MAX_PASSES;
    bool finalPhase = false;
    int progress = 0;
    std::vector<DEFINE> defines = sDefines;
    for (int pass = 0; pass < passes || finalPhase; ++pass) {
        data = withoutExoticStuff(data);
        std::string nextPass;
        //--
        printf("pass:%d [lastPassSize:%d] %s\n", pass, data.length(), finalPhase ? "finalPhase" : "");
        //--
        const int fileEnd = data.length();
        int lineNr = 0;
        int readPos = 0;
        char lc = 0;
        bool inPreprocessor = false;
        int inIf = 0;
        int nextTypeToLookFor = TYPETOLOOKFOR_NONE;
        std::string word;
        int mathClosureLevel = 0;
        //--
        std::string currentDefineString;
        DEFINE currentDefine;
        //--
        std::string additionalComment;
        std::string additionalLineComment;
        bool ifDefineSection = false;
        std::vector<int> commentedOutIndents;
        commentedOutIndents.push_back(0);
        std::map<int, std::string> commentForIndent;
        bool ifDef = false;
        std::string ifSectionDescription;
        //--
        std::string configuration = finalPhase ? FINALPHASE : "";
        bool onlySelectedSections = finalPhase;
        bool addWords = true;
        CLEARADDWORDS // final phase has disabled addwords
        //--
        while (readPos <= fileEnd) {
            progress++;
            if ((progress % 1000) == 0) printf(".");
            if ((progress % (1000 * 40)) == 0) printf("%d\n", progress);
            //--
            char c = GETDATA(readPos);
            if (c == '\n') printf("normal linefeed encountered error\n");
            if (c == REDOCHAR) {
                printf("redo char (c) %c encountered\n", REDOCHAR);
                c = ' ';
            }
            char nc = GETDATA(readPos + 1);
            if (nc == '\n') printf("normal linefeed encoutnered error\n");
            if (nc == REDOCHAR) {
                printf("redo char (nc) %c encountered\n", REDOCHAR);
                nc = ' ';
            }
            if (c == LFCHAR) lineNr++;
            //---
            if (c == '#') { inPreprocessor = true; }
            if (c == LFCHAR && lc != '\\') { inPreprocessor = false; }
            //---
            //--

            if (nextTypeToLookFor == TYPETOLOOKFOR_NONE && (!ifDefineSection)) {
                //-- COMMENTS
                OVERJUMPSLCOMMENT
                OVERJUMPMLCOMMENT
                //-- STRINGS
#if DEFINED_REDUCED_SHADER_PREPROCESSOR_WITH_STRING_RESOLVING != 1
                if (c == '\"') {
                    word += c;
                    readPos++;
                    for (; readPos < fileEnd; ++readPos) {
                        c = GETDATA(readPos);
                        ADDCHAR(word, c);
                        if (c == '\"') { break; }
                    }
                    READPOSUPDATE;
                    continue;
                }
#endif
            }
            //--- MACRO COMPATIBILITY
            const bool macroSharp = (c == '#' && nc == '#' && c != REDOCHAR && nc != REDOCHAR);
            if (macroSharp) {
                readPos++;
                readPos++;
                lc = c;
                continue;
            } // please NO READPOSUPDATE here
            //---

            //--- COLLECT DEFINES
            if (nextTypeToLookFor != TYPETOLOOKFOR_NONE) {
                if (c != REDOCHAR) {
                    ADDWORD(c);
                    while (1) {
                        bool redo = true;
                        const int typeHere = nextTypeToLookFor;
                        switch (typeHere) {
                            case TYPETOLOOKFOR_DEFINEHEADER_START: {
                                if (c != ' ' && c != '\t') nextTypeToLookFor = TYPETOLOOKFOR_DEFINEHEADER_BASE;
                                if (c == LFCHAR && lc != '\\') nextTypeToLookFor = TYPETOLOOKFOR_NONE;
                                break;
                            }
                            case TYPETOLOOKFOR_DEFINEHEADER_BASE: {
                                ADDCHAR(word, c);
                                if (c == '(') {
                                    currentDefine.first.first = trim(word);
                                    nextTypeToLookFor = TYPETOLOOKFOR_DEFINEHEADER_PARAMETERS;
                                    redo = false;
                                    word = "";
                                    break;
                                }
                                if (c == LFCHAR && lc == '\\') {
                                    currentDefine.first.first = trim(word.substr(0, word.length() - 2));
                                    word = "";
                                    nextTypeToLookFor = TYPETOLOOKFOR_DEFINE_CONTENT_START;
                                    redo = false;
                                    break;
                                }
                                if (c == ' ' || c == '\t' || c == LFCHAR) {
                                    currentDefine.first.first = finalPhase ? PHASEDEFINE(trim(word), FINALPHASE) : trim(
                                            word); // it's for fun!!
                                    word = "";
                                    nextTypeToLookFor = TYPETOLOOKFOR_DEFINE_CONTENT_START;
                                }
                                break;
                            }
                            case TYPETOLOOKFOR_DEFINEHEADER_PARAMETERS: {
                                ADDCHAR(word, c);
                                if (c == ')') {
                                    currentDefine.first.second = word;
                                    word = "";
                                    nextTypeToLookFor = TYPETOLOOKFOR_DEFINE_CONTENT_START;
                                    redo = false;
                                    break;
                                }
                                break;
                            }
                            case TYPETOLOOKFOR_DEFINE_CONTENT_START: {
                                if (c != ' ' && c != '\t') nextTypeToLookFor = TYPETOLOOKFOR_DEFINE_CONTENT;
                                if (c == LFCHAR)
                                    nextTypeToLookFor = TYPETOLOOKFOR_DEFINE_CONTENT; // not perfect but whatever
                                break;
                            }
                            case TYPETOLOOKFOR_DEFINE_CONTENT: {
                                if (c == LFCHAR && lc == '\\') {
                                    word = word.substr(0, word.length() - 1);
                                    ADDCHAR(word, c);
                                    redo = false;
                                    break;
                                }
                                if (c == LFCHAR && lc != '\\') {
                                    currentDefine.second = trim(word);
                                    word = "";
                                    bool found = false;
                                    for (int i = 0; i < defines.size(); ++i) {
                                        if (defines[i].first == currentDefine.first) {
                                            defines[i] = currentDefine;
                                            found = true;
                                        }
                                    }
                                    if (!found) {
                                        defines.push_back(currentDefine);
                                    }
                                    std::string defineString = getDefineString(currentDefine);
                                    //printf("#DEFINE %s\n", defineString.c_str());
                                    nextTypeToLookFor = TYPETOLOOKFOR_NONE;
                                    break;
                                }
                                ADDCHAR(word, c);
                                break;
                            }
                            case TYPETOLOOKFOR_DEFINELINE_DONE: {
                                if (c == LFCHAR && lc != '\\') {
                                    nextTypeToLookFor = TYPETOLOOKFOR_NONE;
                                }
                                word = "";
                                break;
                            }
                        }
                        if ((!redo) || (redo && (typeHere == nextTypeToLookFor)))
                            break;
                    }
                }
            }
                //--- NORMAL WORDS
            else {
                MAYBEEVALUATEIFSECTION // not in macro tracings
                if (c == '(') mathClosureLevel++; // macros can have parameters so collect em alltogether
                if (c == ')') mathClosureLevel--;
                if (macroSharp || ((c == '\t' || c == ' ') && mathClosureLevel == 0) || c == LFCHAR) {
                    if (addWords) {
                        word = resolve(word, defines);
                        ADDCHAR(word, c);
                        ADDWORD(word);
                    }
                    word = "";
                } else {
                    ADDCHAR(word, c);
                }
#if DEFINED_MAC_PREPROCESSOR_DEFINES == 1
                if (inPreprocessor && word == "#define") {
                    if (!finalPhase) {
                        ADDWORD(word);
                        word = "";
                        currentDefine = DEFINE();
                        nextTypeToLookFor = TYPETOLOOKFOR_DEFINEHEADER_START;
                    } else {
                        nextTypeToLookFor = TYPETOLOOKFOR_DEFINELINE_DONE;
                    }
                }
#endif
#if DEFINED_MAC_PREPROCESSOR_IFS == 1
                if (inPreprocessor && word == "#if" && nc != 'd') {
                    DEBUGDEFINE;
                    DEFINEINDENT;
                    ifDefineSection = true;
                    inIf++;
                    ADDWORD(word);
                    word = "";
                }
                if (inPreprocessor && word == "#if" && nc == 'd') {
                    DEBUGDEFINE;
                    DEFINEINDENT;
                    ifDefineSection = true;
                    inIf++;
                    ADDWORD(word);
                    word = "";
                    ifDef = true;
                }
                if (inPreprocessor && word == "#else") {
                    ifSectionDescription = commentForIndent[ACTIVATIONINDENTLEVEL]; DEBUGDEFINE;
                    const std::string w1 = COMMENTEDOUTSECTION ? "*/" : "";
                    INVERTSECTION;
                    const std::string w2 = COMMENTEDOUTSECTION ? "/* " + ifSectionDescription + " " : "";
                    ADDWORD(w1);
                    ADDWORD(word);
                    ADDWORD(w2);
                    word = "";
                }
                if (inPreprocessor && word == "#endif") {
                    DEBUGDEFINE;
                    inIf--;
                    if (inIf < 0) printf("macro Error too much #endifs\n");
                    const std::string w1 = COMMENTEDOUTSECTION ? "*/" : "";
                    ADDWORD(w1);
                    ADDWORD(word);
                    word = "";
                    COMMENTINSECTION;
                    CLEARADDWORDS;
                    DEFINEUNINDENT;
                }
#endif
            }
            // --- ADVANCE CHAR POSITION
            READPOSUPDATE
        }
        printf("\n");
        printf("phase completed\n");
        nextPass = exoticStuffAgain(nextPass);
        if (inIf != 0) { printf("too few endifs!\n"); } // MAYBE SOME BUG IN MACRO RESOLVING IS SHOWN BY THIS
        //nextPass = resolveCommentsToStandardC(nextPass, REMOVE_MULTILINE_MULTI_COMMENTS | REMOVE_EMPTY_COMMENTS); // (it's just fun coding, I know that it's not professional here) currently needed here (i just wanted to make some progress here, look back later)
        // -- DEBUG
        stringToFile(nextPass, "c:\\!mad\\Projekte\\Games\\newstuff\\binaryfeatures\\ESBIFT\\chk" + toString(pass));
        stringToFile(allDefinesAsString(defines),
                     "c:\\!mad\\Projekte\\Games\\newstuff\\binaryfeatures\\ESBIFT\\chk_defines" + toString(pass));
        if (finalPhase) break; // ALL DONE
        // -- CHECK IF FILE STAYS THE SAME SO WE REACHED COMPLETION
        if (nextPass.size() == lastPass.size()) {
            bool found = true;
            for (int i = 0; i < lastPass.size(); ++i) {
                if (lastPass[i] != nextPass[i]) {
                    found = false;
                    break;
                }
            }
            if (found) {
                if (phaseDefines(defines, FINALPHASE).empty())
                    break;
                finalPhase = true;
            }
        }
        data = nextPass;
        std::string lastPass = data;
    }
    //nextPass = resolveCommentsToStandardC(nextPass, ERASE_PREPROCESSOR_DIRECTIVES);
    return lastPass;
}
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------
DEFINE FINALPHASEDEFINE(const std::string& name) {
    DEFINE r;
    r.first.first = PHASEDEFINE(name, FINALPHASE);
    r.second = FINALPHASE;
    return r;
}
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------
std::string stringFromArray(const std::vector<unsigned char>& data) { std::string r; for (int i = 0; i < data.size(); ++i) { r += data[i]; } return r; }
std::vector<unsigned char> arrayFromString(const std::string& data) { std::vector<unsigned char> r; for (int i = 0; i < data.length(); ++i) { r.push_back(data[i]); } return r; }
void stringToFile(const std::string& data, const std::string& outputFileName);
std::string stringFromFile(const std::string& fileName);
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------
const std::string resolveBasicMacro(const DEFINE& macro) { return macro.second; }

std::vector<std::string> getParameters(const std::string& parameters) {
    std::vector<std::string> r;
    int pos = 0;
    int closure = 0;
    std::string word;
    pos++; // "(" at front and ")" at end
    while (pos < parameters.size() - 1) {
        char c = parameters[pos];
        if (c == '(') closure++;  if (c == ')') closure--;
        if (c == '{') closure++;  if (c == '}') closure--;
        if (c == '[') closure++;  if (c == ']') closure--;
        if (c == ',' && closure == 0) {
            r.push_back(trim(word));
            word = "";
        }
        else
            ADDCHAR(word,c);
        pos++;
    }
    r.push_back(trim(word));
    return r;
}

void combineVAARGS(std::vector<std::string>& macroParameters, std::vector<std::string>& closureParameters) {
    if (macroParameters[macroParameters.size() - 1] == "...") {
        macroParameters[macroParameters.size() - 1] = "__VA_ARGS__";
        for (int i = macroParameters.size(); i < closureParameters.size(); ++i) {
            closureParameters[macroParameters.size() - 1] += " , " + closureParameters[i];
        }
        closureParameters.resize(macroParameters.size());
    }
}

std::string replaceMacroParameters(const std::string& macroContent, const std::vector<std::string>& sourceParams, const std::vector<std::string>& destParams) {
    if (sourceParams.size() != destParams.size())
        return macroContent;
    std::string currentReplaceState = macroContent;

    std::vector<int> parameterOccurences;
    parameterOccurences.resize(macroContent.size()); // needs 0 init
    for (int i = 0; i < sourceParams.size(); ++i) {
        const std::string& name = sourceParams[i];
        if (macroContent.length() >= name.length()) {
            for (int k = 0; k < (int)macroContent.length() - name.length(); ++k) {
                int found = k;
                for (int j = 0; j < name.length(); ++j) {
                    if (macroContent[k + j] != name[j]) {
                        found = -1;
                        break;
                    }
                }
                if (found != -1) {
                    parameterOccurences[k] = i + 1;
                }
            }
        }
    }

    int suffixPos = 0;
    for (int i = 0; i < macroContent.length(); ++i) {
        int here = parameterOccurences[i];
        if (here == 0)
            continue;
        here--;
        const std::string& name = sourceParams[here];
        if (currentReplaceState.length() >= name.length()) {
            for (int k = suffixPos; k <= (int)currentReplaceState.length() - name.length(); ++k) {
                int found = k;
                for (int j = 0; j < name.length(); ++j) {
                    if (currentReplaceState[k + j] != name[j]) {
                        found = -1;
                        break;
                    }
                }
                if (found != -1) {
                    const std::string prefix = currentReplaceState.substr(0, k);
                    const int endPos = k + name.length();
                    const std::string suffix = currentReplaceState.substr(endPos, currentReplaceState.length() - endPos);
                    currentReplaceState = prefix + destParams[here] + suffix;
                    suffixPos = currentReplaceState.length() - suffix.length();
                    break;
                }
            }
        }
    }
    return currentReplaceState;
}

const std::string resolveClosureMacro(const DEFINE& macro, const std::string& closure) {
    std::vector<std::string> macroParameters = getParameters("(" + macro.first.second);
    std::vector<std::string> closureParameters = getParameters("(" + closure);
    combineVAARGS(macroParameters, closureParameters);
    return replaceMacroParameters(macro.second, macroParameters, closureParameters);
}

const std::string resolved(const DEFINE& macro, const std::string& closure, const std::vector<DEFINE>& defines, int depth = 0) {
    const bool isBasicMacro = macro.first.second.empty();
    if (!isBasicMacro && closure.empty()) printf("macro with parameters but no parameters provided!");
    std::string r = isBasicMacro ? resolveBasicMacro(macro) : resolveClosureMacro(macro, closure);
    return r;
}

const std::string resolve(const std::string& word, const std::vector<DEFINE>& defines) {
    if (word.empty())
        return "";
    std::string currentReplaceState = word;
    int progress = 0;
    const int initialReplaceStateLength = currentReplaceState.length();
    for (int k = 0; k < currentReplaceState.length(); ++k) {
        for (int here = defines.size()-1; here >=0 ; --here) {
            progress++;
            const std::string& name = defines[here].first.first;
            if (currentReplaceState.length() >= name.length() && !(name.empty())) {
                int found = k;
                for (int j = 0; j < name.length(); ++j) {
                    if ((k + j) >= currentReplaceState.length() || currentReplaceState[k + j] != name[j]) {
                        found = -1;
                        break;
                    }
                }
                if (found != -1) {
                    const std::string prefix = currentReplaceState.substr(0, k);
                    std::string closure = "";
                    int endPos = k + name.length();
                    if (!defines[here].first.second.empty()) {
                        int closureCount = 1;
                        int t = endPos;
                        while (closureCount > 0 && endPos < currentReplaceState.length()) {
                            if (currentReplaceState[endPos] == '(') closureCount++; if (currentReplaceState[endPos] == ')') closureCount--;
                            if (currentReplaceState[endPos] == '[') closureCount++; if (currentReplaceState[endPos] == ']') closureCount--;
                            if (currentReplaceState[endPos] == '{') closureCount++; if (currentReplaceState[endPos] == '}') closureCount--;
                            // comments may? be some tiny problem here, yet..
                            endPos++;
                        }
                        closure = currentReplaceState.substr(t, endPos - t);
                    }
                    const std::string suffix = currentReplaceState.substr(endPos,
                        currentReplaceState.length() -
                        endPos);
                    const std::string resl = resolved(defines[here], closure, defines);
                    if (progress > initialReplaceStateLength * defines.size() * MAXRESOLVEITERATIONS) {
                        printf("maybe something went wrong here:%s\n",resl.c_str());
                        progress = 0;
                    }
                    const std::string newReplaceState = prefix + resl;
                    const bool same = newReplaceState == currentReplaceState;
                    currentReplaceState = newReplaceState;
                    k = currentReplaceState.length();
                    currentReplaceState = currentReplaceState + suffix;
                    break;
                }
            }
        }
    }
    return currentReplaceState;
}

DEFINE resolveDefine(const DEFINE &define, const std::vector<DEFINE>& defines) {
    DEFINE r = define;
    r.second = resolve(r.second, defines);
    return r;
}
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------
std::string resolveCommentsToStandardC(const std::string& data, int flags);
//----------------------------------------------------------------------------------------


std::vector<std::string> split(const std::string& h) {
    std::vector<std::string> splitters1 = { " ","\t","\n" };
    std::vector<std::string> splitters2 = { "(",")","[","]","==","=","!=","!",">","<","+","-","*","/",":","?", "&&", "&", "||", "|", "#"};
    std::vector<std::string> splitters = splitters1; ArrayAppend(splitters, splitters2);
    std::vector<std::string> words;
    std::string word;
    for (int i = 0; i < h.length(); ++i) {
        int foundSplitter = -1;
        for (int k = 0; k < splitters.size(); ++k) {
            bool found = true;
            const std::string& v = splitters[k];
            for (int j = 0; j < v.length(); ++j) {
                if ((j + i >= h.length()) || h[j + i] != v[j]) {
                    found = false;
                    break;
                }
            }
            if (found) {
                foundSplitter = k;
                break;
            }
        }
        if (foundSplitter != -1) {
            word = trim(word);
            if (!word.empty())
                words.push_back(word);
            word = "";
            if (foundSplitter >= splitters1.size()) {
                words.push_back(splitters[foundSplitter]);
            }
            i += splitters[foundSplitter].length();
            i--;
        }
        else {
            ADDCHAR(word,h[i]);
        }
    }
    trim(word);
    if (!word.empty())
        words.push_back(word);
    return words;
}

bool isTrue(const std::string& v) {
    return (v == "1") || (v == BTRUE);
}

void printParameters(const std::vector<std::string>& wordsHere) {
    dsprintf("[");
    for (int i = 0; i < wordsHere.size(); ++i) dsprintf("%s,", wordsHere[i].c_str());
    dsprintf("]");
}


std::string evaluateSimpleTerm(const std::vector<std::string>& wordsHere, const std::vector<DEFINE>& defines, bool branch = true, const std::string& configuration = "") {
    //dsprintf("##"); printParameters(wordsHere);
    if (wordsHere.empty()) {
        return BFALSE;
    }
    if (wordsHere[0] == "(") {
        std::vector<std::string> newWordsHere;
        std::vector<std::string> part1;
        int i = 1;
        for (; i < wordsHere.size(); ++i) {
            if (wordsHere[i] == ")") {
                newWordsHere.push_back(evaluateSimpleTerm(part1, defines));
                ++i;
                break;
            }
            dsprintf("%s", wordsHere[i].c_str());
            part1.push_back(wordsHere[i]);
        }
        for (; i < wordsHere.size(); ++i) {
            newWordsHere.push_back(wordsHere[i]);
        }
        return evaluateSimpleTerm(newWordsHere, defines);
    }
    for (int i = 0; i < wordsHere.size(); ++i) {
        if (wordsHere[i] == "&&") {
            std::vector<std::string> a1 = wordsHere; a1.resize(i);
            std::vector<std::string> a2; for (int k = i + 1; k < wordsHere.size(); ++k) a2.push_back(wordsHere[k]);
            return (isTrue(evaluateSimpleTerm(a1, defines)) && isTrue(evaluateSimpleTerm(a2, defines))) ? BTRUE : BFALSE;
        }
        if (wordsHere[i] == "||") {
            std::vector<std::string> a1 = wordsHere; a1.resize(i);
            std::vector<std::string> a2; for (int k = i + 1; k < wordsHere.size(); ++k) a2.push_back(wordsHere[k]);
            return (isTrue(evaluateSimpleTerm(a1, defines)) || isTrue(evaluateSimpleTerm(a2, defines))) ? BTRUE : BFALSE;
        }
        if (wordsHere[i] == "?") {
            std::vector<std::string> a1 = wordsHere; a1.resize(i);
            std::vector<std::string> a2; for (int k = i + 1; k < wordsHere.size(); ++k) a2.push_back(wordsHere[k]);
            const bool check = isTrue(evaluateSimpleTerm(a1, defines));
            return evaluateSimpleTerm(a2, defines, check); 
        }
        if (wordsHere[i] == ":") {
            std::vector<std::string> a1 = wordsHere; a1.resize(i);
            std::vector<std::string> a2; for (int k = i + 1; k < wordsHere.size(); ++k) a2.push_back(wordsHere[k]);
            return branch ? evaluateSimpleTerm(a1, defines) : evaluateSimpleTerm(a2, defines);
        }
    }
    
    if (wordsHere.size() == 1) {
        return wordsHere[0];
    }
    if (wordsHere.size() == 2) {
        if (wordsHere[0] == "def") { // only #ifdef (if used wrongly will produce bugs)
            for (int i = 0; i < defines.size(); ++i) {
                if (defines[i].first.first == (configuration + wordsHere[1])) {
                    return defines[i].second;
                }
            }
            return BFALSE;
        }
    }
    if (wordsHere.size() == 3) {
        if (wordsHere[1] == "==") if (wordsHere[0] == wordsHere[2]) { return BTRUE; }
        if (wordsHere[1] == "!=") if (wordsHere[0] != wordsHere[2]) { return BTRUE; }
        return BFALSE;
    }
    const std::string r = BFALSE;
    dsprintf("%s", r.c_str());
    return r;
}

std::string evaluateSimpleBooleanExpression(const std::string& s, const std::vector<DEFINE>& defines, const std::string& configuration) {
    std::string resolved = resolve(s, defines);
    resolved = exoticStuffAgain(resolved);
    std::vector<std::string> wordsHere = split(resolved);
    dsprintf("%s", s.c_str());
    printParameters(wordsHere);

    const std::string result = evaluateSimpleTerm(wordsHere, defines, true, configuration);
    dsprintf(" -> %s\n", result.c_str());
    return result;
}

std::vector<DEFINE> phaseDefines(const std::vector<DEFINE>& defines, const std::string &phaseId) {
    std::vector<DEFINE> r;
    for (int i = 0; i < defines.size(); ++i) {
        if (ISPHASEDEFINE(defines[i], phaseId))
            r.push_back(defines[i]);
    }
    return r;
}

void preprocessFile(const std::string& inputFileName, const std::string& outputFileName, const std::vector<DEFINE> &sDefines = std::vector<DEFINE>()) {
    std::vector<unsigned char> data;
    {
        FILE* in = fopen(inputFileName.c_str(), "rb");
        fseek(in, 0, SEEK_END);
        int fileLength = ftell(in);
        data.resize(fileLength);
        fseek(in, 0, SEEK_SET);
        fread(&(data[0]), 1, fileLength, in);
        data.push_back(LFCHAR);
        fclose(in);
    }
    data = arrayFromString(preprocessString(stringFromArray(data), sDefines));
    FILE* out = fopen(outputFileName.c_str(), "w");
    fwrite(&(data[0]), 1, data.size(), out);
    fclose(out);
}

//----------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------
std::string removeMultiMultiLineComments(const std::string& data) {
    std::string r;
    int fileEnd = data.length();
    for (int readPos = 0; readPos < fileEnd; ++readPos) {
        char c = data[readPos];
        if (readPos + 1 < fileEnd) {
            char nc = data[readPos + 1];
            if (c == '/' && nc == '*') {
                std::string comment; int ind = 1;
                comment += data[readPos]; readPos++;
                comment += data[readPos]; readPos++;
                for (; readPos < fileEnd - 1; ++readPos)
                {
                    if (data[readPos] == '/' && data[readPos + 1] == '*') {
                        ind++;
                        readPos++;
                        continue;
                    }
                    if (data[readPos] == '*' && data[readPos + 1] == '/')
                    {
                        ind--;
                        if (ind == 0) {
                            comment += data[readPos]; readPos++;
                            comment += data[readPos]; readPos++;
                            break;
                        }
                        readPos++;
                        continue;
                    }
                    comment += data[readPos];
                }
                r += comment;
                readPos--;
                continue;
            }
            else {
                r += c;
            }
        }
        else { r += c; }
    }
    return r;
}

std::string eraseComments(const std::string& data) {
    std::string r;
    int fileEnd = data.length();
    for (int readPos = 0; readPos < fileEnd; ++readPos) {
        char c = data[readPos];
        if (readPos + 1 < fileEnd) {
            char nc = data[readPos + 1];
            if (c == '/' && nc == '/') {
                readPos++;
                readPos++;
                for (; readPos < fileEnd - 1; ++readPos) {
                    if (data[readPos] == LFCHAR) {
                        break;
                    }
                }
                r += LFCHAR;
                continue;
            }
            if (c == '/' && nc == '*') {
                std::string comment;
                int ind = 1;
                readPos++;
                readPos++;
                for (; readPos < fileEnd - 1; ++readPos)
                {
                    if (data[readPos] == '/' && data[readPos + 1] == '*') {
                        ind++;
                        readPos++;
                        continue;
                    }
                    if (data[readPos] == '*' && data[readPos + 1] == '/')
                    {
                        ind--;
                        if (ind == 0) {
                            readPos++;
                            readPos++;
                            break;
                        }
                        readPos++;
                        continue;
                    }
                }
                readPos--;
                continue;
            }
            else {
                r += c;
            }
        }
        else { r += c; }
    }
    return r;
}

// it's just fun coding.. nothing more.. sorry.. normally I work in really big projects and normally I also use libraries and so on.. and c++..

std::string removeEmptyComments(const std::string& data) {
    std::string r;
    int fileEnd = data.length();
    for (int readPos = 0; readPos < fileEnd; ++readPos) {
        char c = data[readPos];
        if (readPos + 1 < fileEnd) {
            char nc = data[readPos + 1];
            if (c == '/' && nc == '/') {
                int readPos2;
                for (readPos2 = readPos; readPos2 < fileEnd - 1; ++readPos2) {
                    if (data[readPos2] == LFCHAR)
                        break;
                }
                int length = readPos2 - readPos;
                if (length > 2)
                    r += data.substr(readPos, length);
                readPos = readPos2;
            } else
            if (c == '/' && nc == '*') {
                int ind = 1;
                int readPos2;
                for (readPos2 = readPos + 1; readPos2 < fileEnd - 1; ++readPos2)
                {
                    if (data[readPos2] == '/' && data[readPos2 + 1] == '*') {
                        ind++;
                        continue;
                    }
                    if (data[readPos2] == '*' && data[readPos2 + 1] == '/')
                    {
                        ind--;
                        if (ind == 0) {
                            readPos2++;
                            break;
                        }
                        continue;
                    }
                }
                int length = readPos2 - readPos;
                r += data.substr(readPos, length);
                readPos = readPos2;
            }
            else {
                r += c;
            }
        }
        else {
            r += c;
        }
    }
    return r;
}

std::string erasePreprocessorDirectives(const std::string& data) {
    std::string r;
    int fileEnd = data.length();
    for (int readPos = 0; readPos < fileEnd; ++readPos) {
        char c = data[readPos];
        if (readPos + 1 < fileEnd) {
            char nc = data[readPos + 1];
            if (c == '#') {
                readPos++;
                readPos++;
                for (; readPos < fileEnd - 1; ++readPos) {
                    if (data[readPos] == LFCHAR && data[readPos-1] != '\\') {
                        break;
                    }
                }
                r += LFCHAR;
                continue;
            }
            r += c;
        }
        else { r += c; }
    }
    return r;
}

std::string convertMultiLineCommentsToSingleLineOnes(const std::string& data) {
    std::string r;
    int fileEnd = data.length();
    for (int readPos = 0; readPos < fileEnd; ++readPos) {
        char c = data[readPos];
        if (readPos + 1 < fileEnd) {
            char nc = data[readPos + 1];
            if (c == '/' && nc == '*') {
                int ind = 1;
                r += "//"; readPos += 2;
                for (; readPos < fileEnd - 1; ++readPos)
                {
                    if (data[readPos] == '/' && data[readPos + 1] == '*') {
                        ind++;
                        readPos++;
                        continue;
                    }
                    if (data[readPos] == '*' && data[readPos + 1] == '/')
                    {
                        ind--;
                        if (ind == 0) {
                            readPos++;
                            readPos++;
                            r += LFCHAR;
                            break;
                        }
                        readPos++;
                        continue;
                    }
                    r += data[readPos];
                    if (data[readPos] == LFCHAR)
                        r += "//";
                }
                readPos--;
                continue;
            }
            else {
                r += c;
            }
        }
        else { r += c; }
    }
    return r;
}

std::string resolveCommentsToStandardC(const std::string& data, int flags) {
    std::string r;
    r = data;
    if (flags & REMOVE_EMPTY_COMMENTS) r = removeEmptyComments(r);
    if (flags & REMOVE_MULTILINE_MULTI_COMMENTS) r = removeMultiMultiLineComments(r);
    if (flags & CONVERT_TO_SINGLE_LINE_COMMENTS) r = convertMultiLineCommentsToSingleLineOnes(r);
    if (flags & ERASE_COMMENTS) r = eraseComments(r);
    if (flags & ERASE_PREPROCESSOR_DIRECTIVES) r = erasePreprocessorDirectives(r);
    return r;
}
//----------------------------------------------------------------------------------------


//----------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------
void stringToFile(const std::string& data, const std::string& outputFileName) {
    std::vector<unsigned char> data2 = arrayFromString(data);
    if (!data2.empty()) {
        FILE* out = fopen(outputFileName.c_str(), "w");
        fwrite(&(data2[0]), 1, data2.size(), out);
        fclose(out);
    }
}

std::string stringFromFile(const std::string& fileName) {
    std::vector<unsigned char> data;
    FILE* in = fopen(fileName.c_str(), "rb");
    fseek(in, 0, SEEK_END);
    int fileLength = ftell(in);
    data.resize(fileLength);
    fseek(in, 0, SEEK_SET);
    fread(&(data[0]), 1, fileLength, in);
    data.push_back(LFCHAR);
    fclose(in);
    return stringFromArray(data);
}

std::string withoutExoticStuff(const std::string& c) {
    std::string r;
    r.resize(c.length()); int i = 0;
    for (int k = 0; k < c.length(); ++k) {
        if (c[k] != 0x0d) {
            r[i++] = (c[k] == 0x0a) ? LFCHAR : c[k];
        }
    }
    r.resize(i);
    return r;
}

std::string exoticStuffAgain(const std::string& c) {
    std::string r;
    r.resize(c.length());
    for (int k = 0; k < c.length(); ++k) {
        char l = c[k];
        if (l == LFCHAR) l = '\n';
        if (l == REDOCHAR) {
            printf("bug encountered (REDOCHAR in outpur)\n");
            l = ' '; // would be a tiny bug maybe worth to look for
        }
        r[k] = l;
    }
    return r;
}

#endif // DEFINED_INCLUDE_REDUCED_SHADER_PREPROCESSOR

//----------------------------------------------------------------------------------------
//-- PREPROCESSOR --------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------

