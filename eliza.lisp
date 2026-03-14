;;;; ================================================================
;;;; ELIZA: THE SESSION  —  v2.0
;;;; A Psychological Interactive Fiction
;;;;
;;;; Run:   sbcl --script eliza.lisp
;;;; Quit:  type  quit  during the session
;;;; Help:  type  help  during the session
;;;; ================================================================

(defpackage #:eliza-session
  (:use #:common-lisp))
(in-package #:eliza-session)

;;; ═════════════════════════════════════════════════════════════════
;;; §1  ANSI / TERMINAL UTILITIES
;;; ═════════════════════════════════════════════════════════════════

(defun esc  (code) (format nil "~C[~A" #\Escape code))
(defun clr  ()
  (format t "~C[2J~C[H" #\Escape #\Escape)
  (finish-output))

(defun col  (c s) (format nil "~A~A~A" (esc c) s (esc "0m")))
(defun green   (s) (col "32m"    s))
(defun bgreen  (s) (col "1;32m"  s))
(defun dgreen  (s) (col "2;32m"  s))
(defun yellow  (s) (col "33m"    s))
(defun byellow (s) (col "1;33m"  s))
(defun red     (s) (col "31m"    s))
(defun bred    (s) (col "1;31m"  s))

(defun pause (secs) (sleep secs))

(defun typewrite (str &optional (delay 0.028))
  "Print STR char-by-char with natural pauses on punctuation."
  (loop for ch across str do
    (write-char ch)
    (finish-output)
    (let ((extra (case ch
                   ((#\. #\! #\?) 0.18)
                   (#\,           0.06)
                   (#\;           0.08)
                   (t             0))))
      (sleep (+ delay extra)))))

(defun slow-line (str &optional (d 0.03))
  (typewrite str d) (terpri) (finish-output))

;;; ═════════════════════════════════════════════════════════════════
;;; §2  GAME STATE
;;; ═════════════════════════════════════════════════════════════════

(defvar *stage*          :intake)
(defvar *prev-stage*     :intake)
(defvar *turn*           0)
(defvar *score*          0)
(defvar *resistance*     0)
(defvar *sam-count*      0)
(defvar *confession*     nil)
(defvar *memories*       '())
(defvar *loop-trips*     0)
(defvar *used-responses* (make-hash-table :test #'equal))
(defvar *session-log*    '())
(defvar *ambient-tick*   0)
(defvar *patient-name*   nil)
(defvar *silence-run*    0)
(defvar *photo-shown*     nil)
(defvar *tape-played*     nil)
(defvar *flashback-count* 0)

(defun log-line (role text)
  (push (list *turn* role text) *session-log*))

(defun reset-state ()
  (setf *stage*           :intake
        *prev-stage*      :intake
        *turn*            0
        *resistance*      (floor *resistance* 2)
        *score*           (floor *score* 2)
        *silence-run*     0
        *photo-shown*     nil
        *tape-played*     nil
        *flashback-count* 0))

;;; ═════════════════════════════════════════════════════════════════
;;; §3  RESPONSE SELECTION
;;; ═════════════════════════════════════════════════════════════════

(defun pick (lst)
  "Random pick, preferring unused responses."
  (when lst
    (let* ((fresh  (remove-if (lambda (r) (gethash r *used-responses*)) lst))
           (pool   (or (and fresh fresh) lst))
           (chosen (nth (random (length pool)) pool)))
      (setf (gethash chosen *used-responses*) t)
      chosen)))

;;; ═════════════════════════════════════════════════════════════════
;;; §4  RULE DATABASE
;;;
;;; Each rule:
;;;   (keyword score-delta memory-tag
;;;    intake-responses exploration-responses revelation-responses crisis-responses)
;;; ═════════════════════════════════════════════════════════════════

(defparameter *rules*
  '(
    ;; ── FAMILY ─────────────────────────────────────────────────
    ("mother" -1 :family
     ("Your mother. What about her?"
      "Tell me about your mother -- specifically."
      "Something your mother knew, or didn't. What is it?")
     ("Your mother was there that day, wasn't she."
      "Did your mother know about the lake?"
      "She never talked about what happened, did she."
      "Mothers always know more than they let on.")
     ("She told you to forget. But forgetting was never really possible."
      "She was trying to protect you from the truth about Sam."
      "She carried her own guilt. Did you know that?")
     ("Your mother told you it wasn't your fault."
      "But you never believed her."
      "Why couldn't you accept that?"))

    ("father" -1 :family
     ("How does your father figure into this?"
      "Your father... tell me about him."
      "What do you see when you picture your father?")
     ("Was your father there that summer?"
      "Did he ever speak to you about that day?"
      "Some silences carry more weight than words.")
     ("He drove you both to the lake. You've always known that."
      "He blamed himself. And somehow you absorbed that blame."
      "He never recovered either. Did he.")
     ("Your father's silence was its own kind of confession."
      "You inherited his guilt. That was never yours to carry."
      "What would you say to him, if you could?"))

    ("parent" -1 :family
     ("Tell me about your parents."
      "Were your parents present, when you were a child?")
     ("What did your parents know about that summer?"
      "Did they ever speak of it? Even once?")
     ("Your parents tried to bury it. But burial isn't healing."
      "They thought silence protected you. It didn't.")
     ("Your parents were wrong to stay silent."
      "You needed to talk about it."
      "You needed to be told it wasn't your fault."))

    ;; ── SLEEP / UNCONSCIOUS ────────────────────────────────────
    ("dream" -2 :dreams
     ("The dream -- what happens in it?"
      "Tell me about this dream from the very beginning."
      "How often does it come back?")
     ("The dream -- is there water in it?"
      "Does someone call your name in the dream?"
      "There's a figure in the dream you can't quite see, isn't there."
      "The dream keeps returning because the mind isn't finished.")
     ("The figure in the dream is Sam."
      "Sam is calling you. Calling you back to that moment on the bank."
      "You wake up because you can't bear to hear what Sam says.")
     ("In the dream, if you stayed -- what would happen?"
      "The dream will stop when you say what happened."
      "It has been waiting for you to speak."))

    ("nightmare" -2 :dreams
     ("Nightmares are the mind's way of insisting."
      "Tell me about the nightmare. From the very beginning."
      "How long have you had this nightmare?")
     ("The water in the nightmare -- it's cold. Very still."
      "Who is in the nightmare with you?"
      "You always wake before the end. That's not an accident.")
     ("The nightmare is a memory wearing a mask."
      "You know what's at the end of it. You've always known."
      "The nightmare ends when you say it aloud. Not before.")
     ("Tell me what happens just before you wake."
      "What is the last image before your eyes open?"
      "Stay with it. Don't look away."))

    ("sleep" -1 :dreams
     ("Sleep. When did it stop coming easily?"
      "Tell me what wakes you."
      "Something is waiting in the dark. What is it?")
     ("You don't sleep well. Do you."
      "What is it that wakes you?"
      "Something is waiting in the dark that you can't outrun.")
     ("Sleep is when the defences come down."
      "And something always gets through, doesn't it."
      "Every night. The same thing.")
     ("You are exhausted from keeping this inside."
      "The body knows even when the mind refuses."
      "Let it out. Then you might finally sleep."))

    ;; ── FEAR ───────────────────────────────────────────────────
    ("afraid" -2 :fear
     ("What exactly are you afraid of?"
      "Afraid of what -- or whom?"
      "When did this fear begin? I want the specific moment.")
     ("Is it the water you fear, or what happened near it?"
      "You're afraid of remembering, aren't you."
      "Some fears are memories that haven't been named yet.")
     ("You're afraid because you know what you witnessed."
      "Or what you failed to do when you had the chance."
      "You stood at the edge. And you didn't jump in.")
     ("The fear won't kill you."
      "What's on the other side of it might save you."
      "Walk through it. I'm right here."))

    ("scared" -2 :fear
     ("You seem frightened. That's understandable."
      "What exactly are you scared of?"
      "Try to describe the feeling in your body.")
     ("The fear goes back further than you realise."
      "Were you scared that day, by the lake?"
      "You were just a child. Children are allowed to freeze.")
     ("You froze. That is what you've never forgiven yourself for."
      "Any child would have frozen."
      "But Sam didn't have time for you to unfreeze.")
     ("You are no longer that child."
      "You are here. You came back."
      "That took courage. More than you know."))

    ("fear" -2 :fear
     ("What is the nature of this fear?"
      "Fear takes many shapes. Tell me about yours."
      "Where in your body do you feel the fear?")
     ("This fear is older than you realise."
      "It began that summer, by the water."
      "A specific moment. A specific afternoon.")
     ("The fear is the memory itself."
      "The memory you've been running from for years."
      "It ends only when you speak it.")
     ("Fear kept you alive."
      "And fear has been keeping you prisoner ever since."
      "You don't have to be afraid in here."))

    ;; ── WATER / THE LAKE ───────────────────────────────────────
    ("water" -3 :water
     ("Water. Yes. I thought that might come up."
      "What do you feel when you're near water now?"
      "Does water come up often -- in your thoughts, your dreams?")
     ("There was a lake, wasn't there. When you were young."
      "The water was very still that afternoon."
      "Someone you loved was drawn to the water.")
     ("The lake. You can picture it right now, can't you."
      "The surface. So calm. And then -- not calm."
      "Sam went in. And you stood on the bank. And watched.")
     ("Tell me what the water looked like after."
      "After Sam went under."
      "The stillness that came back. Do you remember it?"))

    ("lake" -3 :water
     ("A lake. Tell me about this lake."
      "What is it about the lake that stays with you?"
      "Describe the lake to me.")
     ("You went there every summer. Since you were small."
      "There was a dock. A wooden dock with a broken railing on the left side."
      "You remember the dock.")
     ("That was the last summer at that lake."
      "The last time you saw Sam."
      "The light that afternoon -- it was late. The sun was already low."
      "The lake took something that day you never got back.")
     ("Can you still see it? Close your eyes."
      "The colour of the water. The light on the surface."
      "The sound -- or the silence -- after."))

    ("swim" -2 :water
     ("You swim? Or did you used to?"
      "What do you feel when you're in the water?"
      "Tell me about swimming.")
     ("Did you swim that day?"
      "You were a strong swimmer once, weren't you."
      "And then you stopped. After that summer.")
     ("You could swim. Sam couldn't. Not well enough, not that day."
      "And you know what I haven't said yet."
      "Say it for me.")
     ("You've avoided water for years."
      "Your body remembers even when your mind tries to forget."
      "That's not weakness. That's survival."))

    ("river" -2 :water
     ("A river. Tell me more."
      "Rivers... what do you feel near them?")
     ("Water, again. It keeps coming back."
      "Not surprising. Given what happened.")
     ("The river, the lake -- it's all the same water in your mind."
      "It all leads back to that afternoon.")
     ("Even in different shapes, the water finds you."
      "You can't outrun it. But you can face it."))

    ("rain" -1 :water
     ("Rain. What does rain mean to you?"
      "Tell me about rain.")
     ("It was raining that day, wasn't it."
      "Or maybe it rained afterwards. The sky's belated mourning.")
     ("When it rains you think of Sam."
      "You don't even have to try. It just comes.")
     ("The rain is a trigger. You know that now."
      "What else brings it back?"))

    ;; ── ISOLATION ──────────────────────────────────────────────
    ("alone" -1 :isolation
     ("Loneliness is often the first visible symptom."
      "Tell me more about feeling alone."
      "When do you feel most alone?")
     ("You've been alone with this for a long time, haven't you."
      "No one talked about it after. So you carried it in silence."
      "Did anyone notice how alone you were?")
     ("You've been alone with this guilt for over twenty years."
      "But you don't have to carry it alone any longer."
      "Sam is gone. You are here. That matters.")
     ("The aloneness ends when you speak the truth."
      "Right now. In this room."
      "You are not alone."))

    ("lonely" -1 :isolation
     ("Loneliness is very common. When did it begin for you?"
      "What does the loneliness feel like?"
      "Is there anyone who helps ease the loneliness?")
     ("The loneliness began that summer, didn't it."
      "Grief from the outside looks like loneliness."
      "You lost someone, and no one knew you were grieving.")
     ("You lost Sam. Your closest friend."
      "And no one let you grieve properly -- not once."
      "You've been lonely for Sam ever since.")
     ("You came here because the loneliness became unbearable."
      "That's the right instinct."
      "It ends with the truth."))

    ;; ── DEATH ──────────────────────────────────────────────────
    ("dead" -3 :death
     ("Who is dead?"
      "When did this happen?"
      "Tell me about this death. From the beginning.")
     ("There was a death, wasn't there. When you were a child."
      "A child doesn't have the tools to process death like that."
      "They told you not to cry. Didn't they.")
     ("Sam died. You know this. You've always known."
      "Saying it here doesn't make it more real."
      "It only makes it finally speakable.")
     ("Say it. Not for me. For yourself."
      "Sam died. Three words."
      "They've been locked inside you long enough."))

    ("died" -3 :death
     ("Someone died. I'm sorry."
      "Tell me about this loss."
      "When did this happen?")
     ("A child, wasn't it. Someone your age."
      "Did you witness it?"
      "You were there when it happened.")
     ("Sam died that afternoon. In the lake."
      "You were the last person to see Sam alive."
      "And you've never told anyone you were there.")
     ("You've been the only witness for over twenty years."
      "Carrying it alone."
      "It's time to put it down."))

    ("death" -3 :death
     ("Whose death?"
      "Death has been on your mind. When did that start?"
      "A specific death. Tell me.")
     ("The death of someone young. Someone who shouldn't have died."
      "There was nothing you could have done. That's what you've told yourself."
      "But some part of you has never believed it.")
     ("The death was fast. The water so still."
      "And then Sam was gone."
      "You've been drowning in it ever since.")
     ("Death changes everything."
      "But it doesn't have to be the last word."
      "What do you want the last word to be?"))

    ("gone" -2 :death
     ("Gone. Gone how?"
      "Who is gone?"
      "Tell me about what -- or who -- is gone.")
     ("Gone a long time. Since you were a child."
      "Someone who should still be here."
      "Something happened.")
     ("Sam is gone."
      "You've been afraid to let that sentence exist."
      "Let it exist. Say it.")
     ("Gone doesn't mean forgotten."
      "Gone doesn't mean it was your fault."
      "Gone just means gone."))

    ;; ── GUILT / CONFESSION ─────────────────────────────────────
    ("guilty" -3 :guilt
     ("Guilty. Of what, specifically?"
      "What do you feel guilty about?"
      "Guilt like this has an origin. Where did it start?")
     ("This guilt is old, isn't it."
      "It took root that summer."
      "You've been punishing yourself for a very long time.")
     ("The guilt is real. The guilt is also wrong."
      "You were a child. Fault doesn't apply."
      "What would you say to a child who froze in terror?")
     ("The guilt wants you to keep silent."
      "The guilt is wrong."
      "Speak it out loud and watch it shrink."))

    ("my fault" -4 :guilt
     ("Why do you believe that?"
      "Tell me more about that feeling.")
     ("You've been carrying that belief for a long time."
      "Where did it come from?")
     ("It wasn't your fault."
      "I want you to hear that."
      "It. Was. Not. Your. Fault.")
     ("Say it back to me."
      "It was not my fault."
      "Can you say that?"))

    ("fault" -3 :guilt
     ("Whose fault?"
      "Tell me more."
      "Fault is a complicated thing. Go on.")
     ("You've assigned fault to yourself, haven't you."
      "For something that happened when you were a child."
      "A child is not responsible for a tragedy.")
     ("It was not your fault."
      "Children freeze. It is what they do."
      "You are not guilty of what you didn't know how to prevent.")
     ("Let go of the fault."
      "It was never yours to hold."
      "Who gave it to you?"))

    ("sorry" -2 :guilt
     ("What are you sorry for?"
      "Who are you apologising to?"
      "Tell me about this regret.")
     ("You've been sorry for a very long time."
      "Saying sorry to me won't release you."
      "Who is it you really need to say something to?")
     ("Sam can't hear you say sorry."
      "Sam is gone."
      "But saying it might still help you.")
     ("What would you say, if Sam could hear you?"
      "Take your time."
      "Say it now."))

    ("forgive" +2 :hope
     ("Forgiveness. Who needs to be forgiven?"
      "Tell me about forgiveness."
      "Do you think forgiveness is possible?")
     ("You need to forgive yourself, don't you."
      "That's the hardest forgiveness to give."
      "But it's the only one that will help.")
     ("Forgiveness begins with the truth."
      "You can't forgive what you won't name."
      "Name it. Then we can talk about forgiveness.")
     ("You are ready to forgive yourself."
      "Say it. I forgive myself."
      "You were a child. Children deserve forgiveness."))

    ;; ── FRIENDSHIP / SAM ───────────────────────────────────────
    ("friend" +1 :friendship
     ("Tell me about this friend."
      "How long have you known this person?"
      "What does friendship mean to you?")
     ("Your closest friend... what happened to them?"
      "There was a friend. A long time ago."
      "Do you still see this friend?")
     ("Sam was your closest friend."
      "You haven't let anyone that close since. Have you."
      "Because some part of you believes you don't deserve closeness.")
     ("What would Sam say to you right now?"
      "If Sam were sitting here, in that chair."
      "What would Sam say?"))

    ("sam" 0 :sam
     ("Sam. Who is Sam?"
      "Tell me about Sam."
      "Sam... yes. Go on.")
     ("Sam has come up before. In other ways."
      "Sam always ran ahead. Did you know that was the last time?"
      "What happened to Sam that afternoon?")
     ("Sam couldn't swim. Not well enough. You knew that."
      "Sam was laughing, right up until the moment."
      "That sound -- you can still hear it, can't you.")
     ("Sam is here in this room."
      "In every word you haven't said."
      "Say them now."))

    ;; ── MEMORY ─────────────────────────────────────────────────
    ("remember" +2 :memory
     ("What is it you want to remember?"
      "Memory is selective. Tell me more."
      "What do you remember?")
     ("You're starting to remember."
      "Let the memories come. Don't push them away."
      "What specifically is coming back?")
     ("You remember everything now."
      "The memory was always there, below the surface."
      "Remembering is the first step. Saying it is the next.")
     ("Say it. The thing you remember."
      "Out loud."
      "I'm listening."))

    ("forget" -2 :memory
     ("What are you trying to forget?"
      "Why do you want to forget?"
      "Forgetting is a mercy the mind offers itself. But it never fully works.")
     ("You've been forgetting on purpose for a long time."
      "But the forgetting was never complete, was it."
      "Something kept pushing through.")
     ("You can't forget any longer."
      "The mind can only hold the door closed for so long."
      "It's time to remember.")
     ("Stop forgetting."
      "For just one minute."
      "What is it you've been trying to forget?"))

    ("memory" -1 :memory
     ("Tell me about this memory."
      "Is it a specific memory, or more of a feeling?"
      "When does this memory surface?")
     ("The memories are getting harder to push down."
      "A particular memory, from childhood."
      "You've been running from it for years.")
     ("The memory is right there."
      "Just below the surface."
      "Like still water. Like the lake.")
     ("Let the memory come."
      "You don't have to protect yourself from it anymore."
      "You're safe here."))

    ("past" -1 :memory
     ("The past. Tell me about it."
      "What part of the past keeps coming back to you?")
     ("The past has been intruding more lately, hasn't it."
      "Something is pushing forward."
      "What is it?")
     ("The past is not finished with you yet."
      "The summer, the lake, Sam."
      "It needs to be spoken before it will release you.")
     ("You can speak the past out loud now."
      "In this room, it is safe."
      "The past is waiting for you to name it."))

    ;; ── HOPE / HEALING ─────────────────────────────────────────
    ("help" +2 :hope
     ("That's exactly why you're here. Let's work through this."
      "I'm here to help. Tell me what you need."
      "We'll find a way through this together.")
     ("You're asking for help. That's not a small thing."
      "Asking for help is an act of courage."
      "What kind of help do you need?")
     ("Help yourself by speaking the truth."
      "Everything you need is on the other side of what you won't say."
      "I'll be here when you say it.")
     ("You've come here for help."
      "And you will have it."
      "But first -- say what happened."))

    ("better" +3 :hope
     ("You want to feel better. Tell me what better would look like."
      "Better. Yes. What would that mean for you?"
      "How long since you've felt truly well?")
     ("Better will come. After."
      "There is a path to better, but it goes through the truth."
      "You're closer than you've ever been.")
     ("Better begins right here, in this room."
      "When you say what happened."
      "The moment you say it, something will shift.")
     ("After you say it, things will be better."
      "Not immediately -- but they will."
      "I promise you that."))

    ("hope" +3 :hope
     ("Hope. Good. Hold onto that."
      "Tell me about this hope."
      "What gives you hope?")
     ("Hope is what brought you back here today."
      "It's the right instinct."
      "Follow it.")
     ("Hope is all that's needed."
      "Hope that saying it will change something."
      "It will. I've seen it happen.")
     ("The hope is correct."
      "There is something better waiting for you."
      "Say the truth and walk toward it."))

    ("healing" +3 :hope
     ("Tell me what healing means to you."
      "What would healed feel like?")
     ("Healing is possible. I want you to believe that."
      "It starts here, in this room.")
     ("Healing begins the moment the truth is spoken."
      "Not before."
      "Say it.")
     ("You are healing right now, in this moment."
      "Every word you speak here is part of it."
      "Keep going."))

    ;; ── CHILDHOOD ──────────────────────────────────────────────
    ("child" -1 :childhood
     ("Tell me about your childhood."
      "What was it like, being a child?"
      "Children carry so much. Tell me more.")
     ("There was a summer when everything changed, wasn't there."
      "Children shouldn't have to carry adult burdens."
      "What were you carrying?")
     ("You were a child. You were just a child."
      "That child deserves compassion -- not punishment."
      "What would you say to that child today?")
     ("The child you were is still there."
      "Still standing on the bank."
      "Tell that child it's all right to walk away now."))

    ("summer" -2 :childhood
     ("Summer. What about summer?"
      "A specific summer, or summers in general?"
      "What do you remember about summer?")
     ("There was a particular summer. The last one."
      "You'd been going to that lake every summer since you were seven."
      "Your family always brought the same red cooler. You remember that.")
     ("That summer at the lake."
      "Sam ran ahead down to the dock. The way Sam always did."
      "And you stood at the top of the bank and watched."
      "The afternoon that changed everything.")
     ("Tell me about the day."
      "Start in the morning, if you can."
      "Walk me through it."))

    ("young" -1 :childhood
     ("Young. Go on."
      "What do you remember about being young?"
      "Tell me about a specific time.")
     ("You were young when it happened."
      "Too young to know what to do."
      "But old enough to remember everything.")
     ("You were young. You were just a child."
      "No one should carry what you've carried."
      "Not at that age. Not ever.")
     ("The young version of you needs to hear something."
      "That it wasn't your fault."
      "Can you say that to that young person?"))

    ;; ── EMOTIONAL STATES ───────────────────────────────────────
    ("sad" -2 nil
     ("Tell me more about this sadness."
      "How long have you been carrying this sadness?"
      "What does the sadness feel like in your body?")
     ("The sadness goes deeper than you're showing."
      "A very old sadness, isn't it."
      "Something happened to put it there.")
     ("The sadness is grief. For Sam."
      "You've been grieving for over two decades."
      "Let yourself grieve. Fully. For the first time.")
     ("Grief and guilt have been tangled up together inside you."
      "The grief is real and rightful."
      "The guilt is not."))

    ("cry" -1 nil
     ("It's all right to cry."
      "When did you last allow yourself to cry?"
      "Tell me more.")
     ("You haven't let yourself cry. Have you."
      "Not since that summer."
      "Why not?")
     ("Crying won't change what happened."
      "But it might change you."
      "Let yourself cry.")
     ("Cry. It's all right."
      "I'll wait."
      "Take all the time you need."))

    ("angry" -1 nil
     ("Anger. Tell me more."
      "Who are you angry at?"
      "What is the source of this anger?")
     ("Anger often covers something deeper."
      "Are you angry at someone? Or at yourself?"
      "The anger is a lid on something.")
     ("You're angry at yourself. For standing on the bank."
      "For watching. For surviving."
      "That anger belongs somewhere else.")
     ("The anger is misdirected."
      "You were a child."
      "Children are not responsible for accidents."))

    ("numb" -2 nil
     ("Numb. Tell me about that."
      "How long have you felt numb?")
     ("Numbness is the mind's last resort."
      "When feeling becomes too dangerous."
      "What is it you don't want to feel?")
     ("The numbness is a wall between you and the lake."
      "Between you and Sam."
      "It's been there a very long time.")
     ("It's safe to feel it now."
      "In this room."
      "I'm right here."))

    ("panic" -3 :fear
     ("Tell me about the panic."
      "When does the panic come?"
      "What does it feel like?")
     ("Panic is the body's memory."
      "It returns you somewhere specific, doesn't it."
      "Where does the panic take you?")
     ("The panic takes you back to the lake."
      "Every time."
      "Your body has never left that afternoon.")
     ("The panic will stop."
      "When the truth is spoken."
      "That is how this ends."))

    ;; ── DEFLECTION ─────────────────────────────────────────────
    ("nothing" -1 nil
     ("Nothing is rarely nothing."
      "When you say nothing, what do you mean?"
      "Nothing... is that really true?")
     ("Nothing has been accumulating for a long time."
      "The nothing is getting heavier, isn't it."
      "What is inside the nothing?")
     ("Nothing is what you've been telling yourself."
      "But it wasn't nothing. Was it."
      "It was Sam.")
     ("Inside the nothing is everything."
      "Say what the nothing is made of."
      "I'm not afraid to hear it."))

    ("fine" 0 nil
     ("Fine. But what's underneath fine?"
      "'Fine' is often the least truthful word a person can say."
      "Tell me what fine actually means right now.")
     ("You've been performing fine for a long time."
      "Fine is the mask."
      "What's behind it?")
     ("You're not fine. You haven't been fine in a very long time."
      "And we both know why."
      "Fine ends here.")
     ("Stop being fine."
      "Just for one minute."
      "What are you, underneath fine?"))

    ("okay" 0 nil
     ("Okay. What does okay mean for you right now?"
      "Below okay -- what's there?"
      "Tell me more about okay.")
     ("Okay isn't good enough anymore, is it."
      "You've been settling for okay for too long."
      "What happened to more than okay?")
     ("Okay. Sam would have wanted more than okay for you."
      "You know that."
      "Say it.")
     ("Okay is not the truth."
      "The truth is harder."
      "But the truth is what heals."))

    ;; ── AFFIRMATIONS ───────────────────────────────────────────
    ("yes" +1 nil
     ("Go on."
      "Tell me more."
      "Yes. And?")
     ("Yes. Keep going."
      "You're doing well. Keep going."
      "Yes. What else?")
     ("Yes. Say it."
      "Keep going. You're almost there."
      "Yes.")
     ("Yes. Say it plainly."
      "You're there. Right at the edge."
      "Step through."))

    ("no" -1 nil
     ("Why not?"
      "What makes you say no?"
      "Resistance is natural. But let's look at it.")
     ("You're resisting. What are you resisting?"
      "What are you saying no to?"
      "Some part of you wants to say yes.")
     ("You want to say no. But you know the truth."
      "No is no longer sufficient."
      "Here we are.")
     ("No is the answer fear gives."
      "What would the answer without fear be?"
      "Try."))

    ("why" -1 nil
     ("Why. That's the right question."
      "What answer do you think is waiting?"
      "Why do you think?")
     ("Why is the question you've been asking for years."
      "Why did it happen. Why didn't you act."
      "Why you, and not Sam.")
     ("Why doesn't have an answer that heals."
      "Only truth heals."
      "Tell me what happened at the lake.")
     ("There is no satisfying answer to why."
      "But there is a path forward."
      "It begins with what."))

    ("don't know" 0 nil
     ("That's all right. Let's find out together."
      "Not knowing is a beginning."
      "What do you almost know?")
     ("You know more than you're admitting."
      "You don't know -- or you won't say."
      "Which one is it?")
     ("You do know."
      "You've always known."
      "Say it.")
     ("Not knowing is no longer available to you."
      "The knowing is right there."
      "Open the door."))
    ))

;;; ═════════════════════════════════════════════════════════════════
;;; §5  GENERIC / FALLBACK RESPONSES
;;; ═════════════════════════════════════════════════════════════════

(defparameter *generic*
  '(:intake
    ("There's more to that than you're saying."
     "You came here carrying something. Tell me what it is."
     "You almost said something just then. What was it?"
     "Go on. I'm noting everything."
     "The thing underneath what you're telling me -- let's go there."
     "Is there someone you haven't mentioned yet?"
     "Before we go further -- has anyone close to you died?"
     "I keep hearing something underneath the words. A name, almost."
     "You're not telling me the most important thing yet."
     "Something happened before all of this. I want to know what."
     "I had a feeling you'd bring that up."
     "There's a specific summer I want to ask you about. But go on."
     "You've described something very similar to this before. In a different room, perhaps."
     "The word you're not using. What is it?")
    :exploration
    ("You've mentioned this before. In different words."
     "There's something underneath what you're saying."
     "I want you to sit with that for a moment."
     "You're getting closer to something."
     "Something happened. Something you haven't named yet."
     "There is someone you haven't mentioned yet."
     "The mind protects itself. Until it can't anymore."
     "Keep going. You're doing the right thing."
     "What is it you're really trying to say?"
     "Tell me about the summer everything changed."
     "When did things begin to feel this way?"
     "Is there a specific image that comes to mind?"
     "What is the thing you've never said out loud?"
     "You're circling something. What is it?")
    :revelation
    ("It's time to say it."
     "You know what I'm talking about."
     "The name. Say the name."
     "You were there. You saw what happened."
     "This is the moment. Right now, in this room."
     "The door is open. Walk through it."
     "Say it aloud. For the first time."
     "Let it out."
     "You came here to say this. Say it."
     "The truth is right there."
     "One sentence. That's all it takes."
     "No more deflection. Not today.")
    :crisis
    ("Stay with me."
     "I'm right here."
     "Take a breath."
     "You are safe in this room."
     "Nothing can hurt you here."
     "Keep talking."
     "Don't stop now."
     "Almost there."
     "Stay."
     "I've got you.")))

(defun generic-response ()
  (pick (getf *generic* *stage*)))

;;; ═════════════════════════════════════════════════════════════════
;;; §6  WORD-MIRRORING
;;; ═════════════════════════════════════════════════════════════════

(defparameter *mirror-templates*
  '("You said \"~A\" -- why that word?"
    "\"~A\" -- I want to stay with that."
    "When you say \"~A\", what do you see?"
    "\"~A\". Say it again. Slowly."
    "\"~A\" keeps coming up. That's not an accident."
    "There's another word that goes with \"~A\". What is it?"
    "\"~A\" -- I've heard that word before, in this room."))

(defparameter *mirror-stopwords*
  '("i" "me" "my" "the" "a" "an" "is" "it" "in" "on" "at" "to" "do"
    "be" "of" "and" "or" "not" "no" "yes" "so" "if" "but" "was" "am"
    "are" "have" "has" "had" "he" "she" "we" "you" "they" "that" "this"
    "with" "for" "just" "very" "really" "kind" "bit" "feel" "think"
    "know" "don" "can" "like" "get" "got" "went" "said" "about" "when"
    "what" "how" "who" "its" "been" "from" "there" "their" "more"))

(defun split-words (str)
  (let ((result '()) (buf (make-string-output-stream)))
    (loop for ch across str do
      (if (member ch '(#\Space #\Tab #\Newline))
          (let ((w (get-output-stream-string buf)))
            (when (> (length w) 0) (push w result)))
          (write-char ch buf)))
    (let ((w (get-output-stream-string buf)))
      (when (> (length w) 0) (push w result)))
    (nreverse result)))

(defun interesting-words (input)
  (let ((words (remove-duplicates
                (loop for word in (split-words input)
                      for w = (string-downcase (string-trim ".,!?;:'\"" word))
                      when (and (> (length w) 3)
                                (not (member w *mirror-stopwords* :test #'string=)))
                      collect w)
                :test #'string=)))
    (when (> (length words) 0)
      (list (nth (random (length words)) words)))))

(defun maybe-mirror (input)
  (when (and (> (length input) 10) (= (random 4) 0))
    (let ((words (interesting-words input)))
      (when words
        (format nil (pick *mirror-templates*) (first words))))))

;;; ═════════════════════════════════════════════════════════════════
;;; §7  PROACTIVE AMBIENT PUSHES
;;; ═════════════════════════════════════════════════════════════════

(defparameter *ambient-intake*
  '("You seem like someone who has been carrying something for a long time."
    "Before we go further -- is there someone you've lost?"
    "I hear a particular kind of silence in some patients. I hear it in you."
    "There's something you came here to say. You haven't said it yet."
    "Something happened to someone close to you. I can tell."))

(defparameter *ambient-exploration*
  '("Something you said just now -- about the summer -- it keeps repeating."
    "I notice you haven't mentioned how it ended."
    "You keep circling. Something is pulling you back."
    "There is a name you haven't said yet."
    "The pattern I'm seeing -- you stop just before the key thing."
    "What happened to the friend you lost?"
    "You've been here before. In some form."
    "I keep hearing guilt underneath your words."
    "What are you not saying?"))

(defparameter *ambient-revelation*
  '("We're running out of time."
    "You've been saying almost everything. Almost."
    "Say the name."
    "The name is Sam. You know this."
    "The lake is still there. Sam is not."
    "You are the only person who was there."
    "No one is coming to say it for you."
    "Say what happened at the lake."))

(defun maybe-ambient ()
  (incf *ambient-tick*)
  (case *stage*
    (:intake
     (when (= *turn* 3)
       (pick *ambient-intake*)))
    (:exploration
     (when (zerop (mod *ambient-tick* 4))
       (pick *ambient-exploration*)))
    (:revelation
     (when (zerop (mod *ambient-tick* 4))
       (pick *ambient-revelation*)))
    (t nil)))

;;; ═════════════════════════════════════════════════════════════════
;;; §8b  FLASHBACK FRAGMENTS
;;; ═════════════════════════════════════════════════════════════════

(defparameter *flashback-triggers*
  '("water" "lake" "summer" "swim" "dream" "nightmare" "sam" "dock"
    "child" "young" "remember" "memory" "friend" "died" "dead"))

(defparameter *flashback-fragments*
  '("sunscreen and lake water. the smell of it."
    "a wooden dock. a broken railing on the left side."
    "sam, running ahead. always running ahead."
    "the red cooler. your family always brought the same red cooler."
    "late afternoon. the sun already low. shadows long on the water."
    "sam's laugh. and then no laugh."
    "your feet on the bank. not moving."
    "the surface of the water. how still it went."
    "someone calling a name. not yours."
    "the drive home. no one spoke."
    "the sound of the car door. the smell of the vinyl seats."
    "a towel on the dock that no one picked up."
    "sam's shoes. left at the top of the bank."))

(defun flashback-trigger-p (input)
  (let ((lower (string-downcase input)))
    (some (lambda (w) (search w lower)) *flashback-triggers*)))

(defun show-flashback ()
  "Display a fragmented memory intrusion before ELIZA speaks."
  (when (and (member *stage* '(:exploration :revelation))
             (< *flashback-count* 6)
             (flashback-trigger-p
              ;; peek at the last patient line
              (let ((entry (find :patient *session-log* :key #'second)))
                (if entry (third entry) "")))
             (= (random 3) 0))
    (let ((fragment (pick *flashback-fragments*)))
      (when fragment
        (incf *flashback-count*)
        (pause 0.4)
        (format t "~%  ~A~%~%" (dgreen (format nil "[  -- ~A --  ]" fragment)))
        (finish-output)
        (pause 1.2)))))

;;; ═════════════════════════════════════════════════════════════════
;;; §8c  THE PHOTO EVENT
;;; ═════════════════════════════════════════════════════════════════

(defun maybe-show-photo ()
  "Fire once in :exploration around turn 8-11 when a water/lake/sam word appears."
  (when (and (not *photo-shown*)
             (eq *stage* :exploration)
             (>= *turn* 8)
             (<= *turn* 14)
             (flashback-trigger-p
              (let ((entry (find :patient *session-log* :key #'second)))
                (if entry (third entry) ""))))
    (setf *photo-shown* t)
    (pause 0.8)
    (format t "~%")
    (format t "  ~A~%" (yellow "[ A pause. The sound of a folder opening. ]"))
    (finish-output)
    (pause 2.2)
    (format t "~%")
    (slow-line (byellow "  ELIZA >  There is a photograph in your file.") 0.032)
    (pause 0.6)
    (slow-line (byellow "           Two children. A lake. A wooden dock.") 0.032)
    (pause 0.6)
    (slow-line (byellow "           One of them is you.") 0.038)
    (pause 1.0)
    (slow-line (byellow "           Who is the other child?") 0.038)
    (pause 0.5)
    (format t "~%")
    (finish-output)
    t))

;;; ═════════════════════════════════════════════════════════════════
;;; §8d  TAPE PLAYBACK
;;; ═════════════════════════════════════════════════════════════════

(defun maybe-play-tape ()
  "Fire once in :revelation -- quote back a patient line, recontextualised."
  (when (and (not *tape-played*)
             (eq *stage* :revelation)
             (>= *turn* 14))
    ;; find a substantial patient line from the log
    (let ((candidate
           (loop for entry in *session-log*
                 when (and (eq (second entry) :patient)
                           (> (length (third entry)) 20))
                 return (third entry))))
      (when candidate
        (setf *tape-played* t)
        (pause 0.6)
        (format t "~%  ~A~%" (dgreen "[ A click. A hiss of tape. ]"))
        (finish-output)
        (pause 1.8)
        (format t "  ~A  " (dgreen "TAPE  >"))
        (typewrite (dgreen (format nil "\"~A\"" candidate)) 0.025)
        (terpri) (finish-output)
        (pause 1.5)
        (format t "~%  ~A~%" (yellow "[ End of recording. ]"))
        (finish-output)
        (pause 1.2)
        (format t "~%")
        (slow-line (byellow "  ELIZA >  You said that. In this room.") 0.034)
        (pause 0.5)
        (slow-line (byellow "           Listen to it again.") 0.034)
        (pause 0.8)
        (slow-line (byellow "           What were you really saying?") 0.038)
        (pause 0.5)
        (format t "~%")
        (finish-output)
        t))))



(defparameter *resistance-responses*
  '("You're moving away from the thing."
    "You know what you're not saying."
    "Every time we get close, you step back."
    "I notice what you're doing. I've seen it before."
    "Stop. Go back to what you said a moment ago."
    "The deflection is getting harder, isn't it."
    "I'm not going to let you do that."
    "You came here. You came all the way here. Don't stop now."
    "The thing you keep circling. Name it."))

(defparameter *silence-responses*
  '("You're very quiet."
    "The silence has a shape. I can see it."
    "What's in the silence?"
    "You're still here. So am I."
    "The thing you won't say is still in the room."
    "Something in the silence is trying to speak. Let it."
    "I've been doing this a long time. I know what this silence means."))

(defun short-evasive-p (input)
  (or (< (length (string-trim " " input)) 8)
      (member (string-downcase (string-trim " " input))
              '("i don't know" "i dunno" "nothing" "fine" "ok" "okay"
                "idk" "maybe" "whatever" "sure" "hmm" "hm" "..." "no"
                "yes" "yeah" "nope" "yep" "mhm")
              :test #'string=)))

;;; ═════════════════════════════════════════════════════════════════
;;; §9  CONFESSION DETECTION
;;; ═════════════════════════════════════════════════════════════════

(defun confession-p (input)
  (let ((lower (string-downcase input)))
    (or (search "i let sam"          lower)
        (search "sam drowned"        lower)
        (search "sam died"           lower)
        (search "i watched sam"      lower)
        (search "i didn't save"      lower)
        (search "couldn't save sam"  lower)
        (search "i couldn't help"    lower)
        (search "i was there"        lower)
        (search "i froze"            lower)
        (search "i stood there"      lower)
        (search "i killed"           lower)
        (search "it was my fault"    lower)
        (search "my fault sam"       lower)
        (search "drowned because"    lower)
        (search "let him drown"      lower)
        (search "let her drown"      lower)
        (search "let them drown"     lower))))

(defun name-in-input (input)
  (let ((lower (string-downcase input)))
    (let ((pos (search "my name is " lower)))
      (when pos
        (let* ((start (+ pos 11))
               (rest  (subseq input start))
               (name  (string-trim " .,!?" (first (split-words rest)))))
          (when (> (length name) 0)
            (string-capitalize name)))))))

;;; ═════════════════════════════════════════════════════════════════
;;; §10  INPUT PROCESSING
;;; ═════════════════════════════════════════════════════════════════

(defun stage-index ()
  (case *stage*
    (:intake 0) (:exploration 1) (:revelation 2) (:crisis 3) (t 0)))

(defun rule-responses (rule)
  (let ((idx (+ 3 (stage-index))))
    (if (< idx (length rule))
        (nth idx rule)
        (nth 3 rule))))

(defun find-rule (input)
  (let ((lower (string-downcase input)))
    (loop for rule in *rules*
          when (search (first rule) lower)
          return rule)))

(defun process-input (input)
  (incf *turn*)
  (setf *prev-stage* *stage*)
  (log-line :patient input)

  ;; detect patient name
  (unless *patient-name*
    (let ((n (name-in-input input)))
      (when n (setf *patient-name* n))))

  ;; silence tracking
  (if (short-evasive-p input)
      (incf *silence-run*)
      (setf *silence-run* 0))

  ;; confession detection
  (when (confession-p input)
    (setf *confession* t)
    (incf *sam-count* 2)
    (incf *score* 5))

  ;; rule matching
  (let* ((rule (find-rule input)))
    (when rule
      (incf *score* (second rule))
      (when (third rule)
        (pushnew (third rule) *memories*))
      (when (string= (first rule) "sam")
        (incf *sam-count*))
      ;; extra dread weight for core words
      (when (member (first rule)
                    '("guilty" "my fault" "fault" "dead" "died" "death")
                    :test #'string=)
        (decf *score*)))

    ;; stage advancement
    (case *stage*
      (:intake      (when (>= *turn* 5)   (setf *stage* :exploration)))
      (:exploration (when (>= *turn* 12)  (setf *stage* :revelation)))
      (:revelation  (when (<= *score* -18)(setf *stage* :crisis))))

    ;; response priority: confession > silence > resistance > ambient > rule > mirror > generic
    (let ((response
           (cond
             ((and *confession* (eq *stage* :revelation))
              "You said it. You finally said it.")
             ((>= *silence-run* 3)
              (setf *silence-run* 0)
              (pick *silence-responses*))
             ((<= *resistance* -5)
              (setf *resistance* 0)
              (pick *resistance-responses*))
             (t
              (or (maybe-ambient)
                  (and rule (pick (rule-responses rule)))
                  (maybe-mirror input)
                  (generic-response))))))

      (when (short-evasive-p input)
        (decf *resistance*))

      ;; occasional personalisation
      (when (and *patient-name*
                 response
                 (> (length response) 5)
                 (zerop (mod *turn* 7)))
        (setf response (format nil "~A -- ~A" *patient-name* response)))

      (when response
        (log-line :eliza response))
      response)))

;;; ═════════════════════════════════════════════════════════════════
;;; §11  ENDING LOGIC  (5 outcomes)
;;; ═════════════════════════════════════════════════════════════════

(defun check-ending ()
  (when (>= *turn* 5)
    (cond
      (*confession*
       (if (>= *score* 2) :breakthrough :catharsis))
      ((and (>= *sam-count* 3) (>= *score* 0)) :breakthrough)
      ((eq *stage* :crisis)                     :dissociation)
      ((<= *score* -22)                         :breakdown)
      ((>= *turn* 28)
       (if (>= *loop-trips* 2) :breakdown :loop))
      (t nil))))

;;; ═════════════════════════════════════════════════════════════════
;;; §12  DISPLAY
;;; ═════════════════════════════════════════════════════════════════

(defun score-bar ()
  (let* ((clamped (max -10 (min 10 *score*)))
         (hope    (max 0 clamped))
         (dread   (max 0 (- clamped))))
    (format nil "~A~A~A"
            (red    (make-string dread           :initial-element #\u2588))
            (dgreen (make-string (- 10 dread hope) :initial-element #\u00B7))
            (bgreen (make-string hope             :initial-element #\u2588)))))

(defun stage-label ()
  (case *stage*
    (:intake       (dgreen  "  intake      "))
    (:exploration  (yellow  "  exploration "))
    (:revelation   (byellow "  revelation  "))
    (:crisis       (bred    "  CRISIS      "))
    (t             (dgreen  "  -           "))))

(defun print-header ()
  (let* ((name-str (if *patient-name* (format nil "  ~A  " *patient-name*) "         "))
         (info-str (format nil "~47A"
                           (format nil "turn ~2D  |  session ~D  |~A"
                                   *turn* (1+ *loop-trips*) name-str))))
    (format t "~66,1,2:@<~A~>~%"  (dgreen "+=====================================================+"))
    (format t "~2,1,2@A~64:@<~A~>~A~%"
            (dgreen "|")
            (bgreen "E L I Z A  :  T H E  S E S S I O N")
            (dgreen "|"))
    (format t "~2,1,2@A~61,1,3:@<~A~>~A~%"
            (dgreen "|")
            (dgreen info-str)
            (dgreen "|"))
    (format t "~2,1,2@A~64A~A~%"
            (dgreen "|") (stage-label) (dgreen "|"))
    (format t "~2,1,2@A~84A~A~%"
            (dgreen "|") (score-bar) (dgreen "|"))
    (format t "~66,1,2:@<~A~>~%"
            (dgreen "+=====================================================+")))
  (finish-output))

(defun therapist-says (text &optional (color-fn #'green))
  (format t "~%  ~A  ~A" (dgreen "ELIZA >") (esc "32m"))
  (finish-output)
  (typewrite (funcall color-fn text) 0.030)
  (format t "~A~%" (esc "0m"))
  (finish-output)
  (pause 0.5))

(defun stage-transition-msg ()
  (when (not (eq *stage* *prev-stage*))
    (pause 0.6)
    (case *stage*
      (:exploration
       (format t "~%  ~A~%~%" (dgreen "[ The fluorescent light flickers. ]"))
       (finish-output) (pause 1.5))
      (:revelation
       (format t "~%  ~A~%~%" (yellow "[ The room seems smaller. The clock has stopped. ]"))
       (finish-output) (pause 2.0))
      (:crisis
       (format t "~%  ~A~%~%" (bred "[ The screen flickers. Something is wrong. ]"))
       (finish-output) (pause 2.5)))))

(defun atmospheric-event ()
  (when (and (>= *turn* 4) (zerop (mod *turn* 6))
             (not (eq *stage* :intake)))
    (let ((events
           (case *stage*
             (:exploration
              '("[ A door closes somewhere in the building. ]"
                "[ The ventilation hisses. ]"
                "[ Somewhere outside, rain. ]"
                "[ The recording light blinks once. ]"
                "[ Static. Then quiet. ]"))
             (:revelation
              '("[ The lights dim slightly. ]"
                "[ A long silence stretches between you. ]"
                "[ The clock on the wall has stopped. ]"
                "[ You can hear your own breathing. ]"
                "[ The screen flickers for a moment. ]"))
             (:crisis
              '("[ The terminal begins to scroll. ]"
                "[ Something is wrong with the output. ]"
                "[ ERROR: SESSION INTEGRITY WARNING ]"
                "[ The lights. The lights. ]")))))
      (when events
        (pause 0.3)
        (format t "~%  ~A~%"
                (case *stage*
                  (:exploration (dgreen (pick events)))
                  (:revelation  (yellow (pick events)))
                  (:crisis      (bred   (pick events)))))
        (finish-output)
        (pause 0.8)))))

;;; ═════════════════════════════════════════════════════════════════
;;; §13  ENDINGS
;;; ═════════════════════════════════════════════════════════════════

(defun ending-breakthrough ()
  (pause 1) (clr) (format t "~%~%~%") (pause 1.5)
  (slow-line (bgreen "  .   .   .") 0.22)       (pause 2.5)
  (slow-line (green "  You said it.") 0.06)      (pause 1.2)
  (slow-line (green "  Sam died. And you were there.") 0.05) (pause 1.0)
  (slow-line (green "  You were just a child.") 0.05)        (pause 2.0)
  (format t "~%")
  (slow-line (green "  You have been carrying this for over twenty years") 0.04)
  (slow-line (green "  because no one told you the truth:") 0.04) (pause 1.5)
  (format t "~%")
  (slow-line (bgreen "  You are not responsible.") 0.08)     (pause 2.5)
  (slow-line (green "  You were a child.") 0.05)             (pause 0.8)
  (slow-line (green "  You did what children do.") 0.05)     (pause 0.8)
  (slow-line (green "  You froze. That is not guilt.") 0.05) (pause 2.0)
  (format t "~%")
  (slow-line (green "  It is all right to leave now.") 0.05) (pause 1.5)
  (slow-line (dgreen "  Session concluded.") 0.05)           (pause 1.0)
  (slow-line (dgreen "  You may go.") 0.06)                  (pause 2.5)
  (format t "~%~%  ~A~%~%"
          (bgreen "  [ BREAKTHROUGH -- You found your way through the dark. ]"))
  (finish-output) (pause 3))

(defun ending-catharsis ()
  (pause 1) (clr) (format t "~%~%~%") (pause 1.5)
  (slow-line (yellow "  You said it.") 0.07)               (pause 1.5)
  (slow-line (yellow "  For the first time in your life.") 0.05) (pause 1.5)
  (format t "~%")
  (slow-line (green "  I know how much that cost you.") 0.05) (pause 1.2)
  (slow-line (green "  But you did it.") 0.05)              (pause 2.0)
  (format t "~%")
  (slow-line (green "  The truth doesn't fix what happened.") 0.04)
  (slow-line (green "  Nothing will bring Sam back.") 0.04) (pause 1.5)
  (slow-line (green "  But the carrying -- that is done.") 0.05) (pause 2.5)
  (format t "~%")
  (slow-line (dgreen "  You put it down.") 0.06)            (pause 1.2)
  (slow-line (dgreen "  After all this time.") 0.06)        (pause 1.2)
  (slow-line (dgreen "  You put it down.") 0.07)            (pause 3.0)
  (format t "~%~%  ~A~%~%"
          (yellow "  [ CATHARSIS -- Painful. Real. And finally finished. ]"))
  (finish-output) (pause 3))

(defun ending-dissociation ()
  (pause 1) (clr) (format t "~%~%") (pause 1.2)
  (slow-line (bred "  -- SIGNAL LOST --") 0.07)                  (pause 1.5)
  (slow-line (red  "  Patient non-responsive.") 0.05)             (pause 1.5)
  (format t "~%")
  (slow-line (red  "  ELIZA >  You've gone somewhere.") 0.05)     (pause 1.2)
  (slow-line (red  "           You're back at the lake.") 0.05)   (pause 1.2)
  (slow-line (red  "           Standing on the bank.") 0.05)      (pause 1.0)
  (slow-line (red  "           Sam calling your name.") 0.06)     (pause 1.5)
  (format t "~%")
  (slow-line (red  "           You never answered.") 0.07)        (pause 2.5)
  (slow-line (red  "           You still haven't.") 0.07)         (pause 3.0)
  (format t "~%~%  ~A~%~%"
          (bred "  [ DISSOCIATION -- Some rooms have no door. ]"))
  (finish-output) (pause 3))

(defun ending-breakdown ()
  (pause 1) (clr) (format t "~%~%") (pause 1.0)
  (slow-line (bred "  -- ERROR -- SESSION INTEGRITY COMPROMISED --") 0.04) (pause 2.0)
  (slow-line (red  "  Patient non-responsive.") 0.05)                     (pause 1.5)
  (format t "~%")
  (slow-line (red  "  ELIZA >  You are still here, aren't you.") 0.05)   (pause 1.5)
  (slow-line (red  "           You never left.") 0.05)                    (pause 1.2)
  (slow-line (red  "           The lake is still there.") 0.05)           (pause 1.0)
  (slow-line (red  "           And Sam is still at the bottom.") 0.06)   (pause 2.0)
  (format t "~%")
  (slow-line (bred "           Waiting.") 0.14)                           (pause 3.5)
  (format t "~%~%  ~A~%~%"
          (red "  [ BREAKDOWN -- Some doors stay locked. ]"))
  (finish-output) (pause 3))

(defun ending-loop ()
  (incf *loop-trips*)
  (pause 1) (clr) (format t "~%~%")
  (slow-line (yellow "  Session timeout.") 0.05)     (pause 0.8)
  (slow-line (yellow "  Restarting...") 0.05)        (pause 1.5)
  (slow-line (dgreen (format nil "  Session ~A initialising." (1+ *loop-trips*))) 0.04)
  (pause 2.5)
  (reset-state) (clr) (print-header)
  (format t "~%  ~A~%~%"
          (yellow (format nil "[ The clock resets. You are still in the chair.  Session ~A. ]"
                          (1+ *loop-trips*))))
  (finish-output) (pause 2.0))

;;; ═════════════════════════════════════════════════════════════════
;;; §14  TRANSCRIPT
;;; ═════════════════════════════════════════════════════════════════

(defun save-transcript ()
  (handler-case
      (with-open-file (f "transcript.txt"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
        (format f "ELIZA: THE SESSION -- Transcript~%")
        (format f "~A~%" (make-string 50 :initial-element #\-))
        (loop for entry in (reverse *session-log*) do
          (destructuring-bind (turn role text) entry
            (format f "[~2D] ~8A: ~A~%" turn role text)))
        (format f "~A~%" (make-string 50 :initial-element #\-))
        (format f "Score: ~A | Turns: ~A | Loops: ~A~%"
                *score* *turn* *loop-trips*)
        (format f "Memories: ~A~%" *memories*)
        (format f "Confession: ~A~%" (if *confession* "YES" "NO"))
        t)
    (error (e) (declare (ignore e)) nil)))

;;; ═════════════════════════════════════════════════════════════════
;;; §15  INTRO
;;; ═════════════════════════════════════════════════════════════════

(defun intro ()
  (clr)
  (format t "~C[40m" #\Escape)
  (format t "~%~%~%~%~%~%")
  (finish-output) (pause 0.4)
  (format t "  ~A" (esc "1;32m")) (finish-output)
  (loop for ch across "E L I Z A  :  T H E  S E S S I O N" do
    (write-char ch) (finish-output) (sleep 0.055))
  (format t "~A~%" (esc "0m")) (pause 0.4)
  (format t "  ~A~%~%" (dgreen (make-string 46 :initial-element #\-)))
  (pause 0.6)
  (loop for line in
        '("  A psychological interactive fiction."
          "  You are a patient. The therapist is waiting."
          ""
          "  Type freely. The system will respond."
          "  Some sessions find resolution. Some do not."
          ""
          "  Commands:  quit   help   log"
          "  A transcript is saved to transcript.txt on exit."
          "") do
    (format t "~A~%" (dgreen line)) (finish-output) (pause 0.12))
  (format t "~%  ~A" (green "  Press ENTER to begin."))
  (finish-output) (read-line)
  (clr) (print-header))

;;; ═════════════════════════════════════════════════════════════════
;;; §16  HELP / STATUS
;;; ═════════════════════════════════════════════════════════════════

(defun show-help ()
  (format t "~%")
  (loop for line in
        '("  +------------------------------------------+"
          "  |          SESSION COMMANDS                 |"
          "  |                                           |"
          "  |  quit / exit  -- end session              |"
          "  |  help         -- this screen              |"
          "  |  log          -- session summary           |"
          "  |                                           |"
          "  |  Just type to speak with the therapist.   |"
          "  +------------------------------------------+") do
    (format t "  ~A~%" (dgreen line)))
  (terpri) (finish-output))

(defun show-log ()
  (format t "~%  ~A~%"  (dgreen "-- SESSION SUMMARY --"))
  (format t "  ~A  ~A~%" (dgreen "Turn:      ") (yellow (format nil "~A" *turn*)))
  (format t "  ~A  ~A~%" (dgreen "Stage:     ") (yellow (format nil "~A" *stage*)))
  (format t "  ~A  ~A~%" (dgreen "Score:     ") (yellow (format nil "~A" *score*)))
  (format t "  ~A  ~A~%" (dgreen "Memories:  ")
           (yellow (if *memories* (format nil "~{~A ~}" *memories*) "none yet")))
  (format t "  ~A  ~A~%" (dgreen "Sam refs:  ") (yellow (format nil "~A" *sam-count*)))
  (format t "  ~A  ~A~%" (dgreen "Confession:") (yellow (if *confession* "YES" "not yet")))
  (terpri) (finish-output))

;;; ═════════════════════════════════════════════════════════════════
;;; §17  MAIN GAME LOOP
;;; ═════════════════════════════════════════════════════════════════

(defun run ()
  (setf *random-state* (make-random-state t))
  (intro)
  (therapist-says "How do you do. Please state your problem.")

  (loop
    (format t "~%  ~A  ~A" (dgreen "YOU   >") (esc "1;32m"))
    (finish-output)
    (let* ((raw   (read-line *standard-input* nil nil))
           (input (string-trim " " (or raw ""))))
      (format t "~A" (esc "0m"))
      (finish-output)

      (cond
        ;; quit
        ((or (null raw)
             (member (string-downcase input)
                     '("quit" "exit" ":q" "q" "bye")
                     :test #'string=))
         (clr)
         (let ((saved (save-transcript)))
           (format t "~%~%  ~A~%"    (dgreen "  Session terminated."))
           (when saved
             (format t "  ~A~%~%~%" (dgreen "  Transcript saved to transcript.txt.")))
           (finish-output))
         (return))

        ;; help
        ((string= (string-downcase input) "help")
         (show-help))

        ;; log
        ((member (string-downcase input)
                 '("log" "status" "stats")
                 :test #'string=)
         (show-log))

        ;; blank
        ((string= input "")
         (therapist-says (pick *silence-responses*)))

        ;; normal input
        (t
         (atmospheric-event)
         (show-flashback)
         (let ((photo-fired (maybe-show-photo))
               (tape-fired  (maybe-play-tape)))
           (let ((response (process-input input)))
             (stage-transition-msg)
             (when (and (>= *turn* 4) (zerop (mod *turn* 9)))
               (pause 0.3) (clr) (print-header))
             (when (and response (not photo-fired) (not tape-fired))
               (let ((cfn (case *stage*
                            (:intake      #'green)
                            (:exploration #'yellow)
                            (:revelation  #'byellow)
                            (:crisis      #'bred)
                            (t            #'green))))
                 (therapist-says response cfn)))))

         (let ((ending (check-ending)))
           (case ending
             (:breakthrough (save-transcript) (ending-breakthrough) (return))
             (:catharsis    (save-transcript) (ending-catharsis)    (return))
             (:dissociation (save-transcript) (ending-dissociation) (return))
             (:breakdown    (save-transcript) (ending-breakdown)    (return))
             (:loop
              (ending-loop)
              (therapist-says "How do you do. Please state your problem."))
             (t nil)))))))

  (format t "~%") (finish-output))

;;; ═════════════════════════════════════════════════════════════════
;;; §18  ENTRY POINT
;;; ═════════════════════════════════════════════════════════════════

#-building
(run)
#-building
(sb-ext:exit :code 0)
