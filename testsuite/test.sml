use "Main.sml";
Prolog (parse "init.");

(* Basic preferences *)
Prolog (parse "enjoys(tom,coffee).");
Prolog (parse "enjoys(tom,cigars).");
Prolog (parse "enjoys(tom,football).");

Prolog (parse "enjoys(lisa,running).");
Prolog (parse "enjoys(lisa,smoothies).");
Prolog (parse "enjoys(lisa,cycling).");
Prolog (parse "enjoys(lisa,progressive_news).");

Prolog (parse "enjoys(karen,running).");
Prolog (parse "enjoys(karen,smoothies).");
Prolog (parse "enjoys(karen,cycling).");
Prolog (parse "enjoys(karen,conservative_radio).");

(* Rule definitions *)
Prolog (parse "fitness_fan(X) :- enjoys(X,smoothies), enjoys(X,running).");
Prolog (parse "liberal(X) :- enjoys(X,progressive_news).");
Prolog (parse "conservative(X) :- enjoys(X,conservative_radio).");
Prolog (parse "party_guy(X) :- enjoys(X,cigars).");

(* Queries *)
Prolog (parse "enjoys(tom,Y)?");
Prolog (parse "fitness_fan(Person)?");
Prolog (parse "liberal(Z), party_guy(Z)?");
