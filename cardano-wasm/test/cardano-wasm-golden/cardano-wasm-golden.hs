{-# LANGUAGE CPP #-}

#if !defined(wasm32_HOST_ARCH)
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --hide-successes #-}
#else
main = return ()
#endif
