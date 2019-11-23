module Commands.Concept (concept) where
    import Args (ConceptArgs)

    concept :: ConceptArgs -> IO ()
    concept _ = putStrLn "Concept"
