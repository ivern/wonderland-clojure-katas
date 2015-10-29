(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= 0 1)))
  (testing "queens are higher rank than jacks"
    (is (> (play-round [:spade :queen] [:spade :jack]) 0)))
  (testing "kings are higher rank than queens"
    (is (> (play-round [:spade :king] [:spade :queen]) 0)))
  (testing "aces are higher rank than kings"
    (is (> (play-round [:spade :ace] [:spade :king]) 0)))
  (testing "if the ranks are equal, clubs beat spades"
    (is (> (play-round [:club :queen] [:spade :queen]) 0)))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (> (play-round [:diamond :queen] [:club :queen]) 0)))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (> (play-round [:heart :queen] [:diamond :queen]) 0))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"))

