// Venatus loads Google's consent dialog, which works through the queue at
// googlefc.callbackQueue once it has loaded and runs whatever is pushed after
// at once.
export const showConsentSettings = () => {
    self.googlefc = self.googlefc || {};
    self.googlefc.callbackQueue = self.googlefc.callbackQueue || [];
    self.googlefc.callbackQueue.push(() => self.googlefc.showRevocationMessage());
};
