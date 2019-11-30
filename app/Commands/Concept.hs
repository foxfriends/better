module Commands.Concept (concept) where
    import Args (ConceptArgs)

    concept :: ConceptArgs -> IO (Either String ())
    concept _ = do
        putStrLn "Concept"
        return $ Right ()
