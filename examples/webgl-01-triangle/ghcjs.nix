{ reflex-platform,  ... }: reflex-platform.ghcjs.override {
    overrides = self: super: {
        bothgl = self.callPackage ../../. {};
    };
}
