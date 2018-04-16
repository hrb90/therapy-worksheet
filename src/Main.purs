module Main where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array as Array
import Data.List as List
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..), fst)
import Data.Validation.Semigroup (V, invalid)
import Flare (Label, UI, intSlider, resizableList, runFlareShow, select, textarea, textarea_)
import Signal.Channel (CHANNEL)

newtype Location = Location String
derive instance newtypeLocation :: Newtype Location _
derive newtype instance showLocation :: Show Location

newtype Activity = Activity String
derive instance newtypeActivity :: Newtype Activity _
derive newtype instance showActivity :: Show Activity

newtype WhoWith = WhoWith String
derive instance newtypeWhoWith :: Newtype WhoWith _
derive newtype instance showWhoWith :: Show WhoWith

newtype Situation = Situation {
  location :: Location,
  activity :: Activity,
  whowith :: WhoWith
}

instance showSituation :: Show Situation where
  show (Situation { location, activity, whowith }) = 
    "Location: " <> show location <> "\n" <>
    "Activity: " <> show activity <> "\n" <>
    "With: " <> show whowith <> "\n"

data Emotion =
  Sad
  | Scared
  | Angry

derive instance eqEmotion :: Eq Emotion

instance showEmotion :: Show Emotion where
  show Sad = "Sad"
  show Scared = "Scared"
  show Angry = "Angry"

type EmotionState = Tuple Emotion Int

newtype Thought = Thought String
derive instance newtypeThought :: Newtype Thought _
derive newtype instance showThought :: Show Thought

newtype Evidence = Evidence String
derive instance newtypeEvidence :: Newtype Evidence _
derive newtype instance showEvidence :: Show Evidence

newtype CBTForm = Form {
  situation :: Situation,
  negativeThought :: Thought,
  initialMoods :: Array EmotionState,
  evidenceFor :: Array Evidence,
  evidenceAgainst :: Array Evidence,
  newThought :: Thought,
  newMoods :: Array EmotionState
}

instance showCbtForm :: Show CBTForm where
  show (Form { situation, negativeThought, initialMoods, evidenceFor, evidenceAgainst, newThought, newMoods }) =
    show situation
    <> "Initial thought: " <> show negativeThought <> "\n"
    <> "Initial moods: " <> show initialMoods <> "\n"
    <> "Evidence for the thought: " <> show evidenceFor <> "\n"
    <> "Evidence against the thought: " <> show evidenceAgainst <> "\n"
    <> "New thought: " <> show newThought <> "\n"
    <> "New moods: " <> show newMoods

newtype ValidationError = ValidationError String
derive instance newtypeValidationError :: Newtype ValidationError _ 
derive newtype instance showValidationError :: Show ValidationError

type Validated a = V (Array ValidationError) a

invalid' :: forall a. String -> Validated a
invalid' msg = invalid $ [ValidationError msg]

validateArrayNotEmpty :: forall a. String -> Array a -> Validated (Array a)
validateArrayNotEmpty label [] = invalid' $ label <> " should have at least one member"
validateArrayNotEmpty _ arr = pure arr

validateMoodsMatch :: Array EmotionState -> Array EmotionState -> Validated (Array EmotionState)
validateMoodsMatch init final
  | map fst init == map fst final = pure final
  | otherwise = invalid' "Lists of moods should match"

validateStrNotEmpty :: forall n. Newtype n String => String -> n -> Validated n
validateStrNotEmpty label = map wrap <<< validate label <<< unwrap
  where
    validate label str
      | str == "" = invalid' $ label <> " should be a nonempty string"
      | otherwise = pure str

buildSituation :: Location -> Activity -> WhoWith -> Situation
buildSituation location activity whowith =
  Situation {
    location: location,
    activity: activity,
    whowith: whowith
  }

validateSituation :: Situation -> Validated Situation
validateSituation (Situation { location, activity, whowith }) =
  buildSituation
  <$> validateStrNotEmpty "Location" location
  <*> validateStrNotEmpty "Activity" activity
  <*> pure whowith

validateForm 
  :: Situation 
  -> Thought 
  -> Array EmotionState 
  -> Array Evidence 
  -> Array Evidence
  -> Thought
  -> Array EmotionState 
  -> Validated CBTForm
validateForm sit negThought initMoods evFor evAgainst newThought newMoods =
  buildCbtForm
  <$> validateSituation sit
  <*> validateStrNotEmpty "Initial thought" negThought
  <*> validateArrayNotEmpty "List of moods" initMoods
  <*> validateArrayNotEmpty "Evidence for" evFor
  <*> validateArrayNotEmpty "Evidence against" evAgainst
  <*> validateStrNotEmpty "Final thought" newThought
  <*> validateMoodsMatch initMoods newMoods
  where 
    buildCbtForm sit negThought initMoods evFor evAgainst newThought newMoods = Form $
      { situation: sit
      , negativeThought: negThought 
      , initialMoods: initMoods
      , evidenceFor: evFor
      , evidenceAgainst: evAgainst
      , newThought: newThought
      , newMoods: newMoods 
      }

moodDropDown :: forall a. UI a Emotion
moodDropDown = select "Mood" emotions show
  where emotions = Sad :| [Scared, Angry]

moodSlider :: forall a. UI a Int
moodSlider = intSlider "Level" 0 100 50

moodWidget :: forall a. UI a EmotionState
moodWidget = lift2 Tuple moodDropDown moodSlider

emptyTextArea :: forall a b. Newtype b String => Label -> UI a b
emptyTextArea = map wrap <<< flip textarea ""

situationInput :: forall a. UI a Situation
situationInput =
  buildSituation
  <$> emptyTextArea "Where are you?"
  <*> emptyTextArea "What are you doing?"
  <*> emptyTextArea "Who are you with?"

initialThoughtInput :: forall a. UI a Thought
initialThoughtInput = emptyTextArea "What negative thought was going through your mind?"

alternativeThoughtInput :: forall a. UI a Thought
alternativeThoughtInput = emptyTextArea "Write a new thought that takes into account the evidence for and against the original thought."

evidenceList :: forall a. Label -> UI a (Array Evidence)
evidenceList label =
  (map wrap)
  <$> Array.fromFoldable
  <$> resizableList 
        label
        (const textarea_ "")
        ""
        (List.fromFoldable [])

moodList :: forall a. Label -> UI a (Array EmotionState)
moodList label =
  Array.fromFoldable
  <$> resizableList
        label
        (const moodWidget)
        (Tuple Sad 50)
        (List.fromFoldable [])

bigMood :: forall a. UI a (Validated CBTForm)
bigMood =
  validateForm
  <$> situationInput
  <*> initialThoughtInput
  <*> moodList "What are you feeling?"
  <*> evidenceList "What evidence supports the thought?"
  <*> evidenceList "What evidence indicates that this thought might not be true all the time?"
  <*> alternativeThoughtInput
  <*> moodList "How do you feel about the situation now?"

main :: forall e. Eff (dom :: DOM, channel :: CHANNEL | e) Unit
main = runFlareShow "input" "output" bigMood