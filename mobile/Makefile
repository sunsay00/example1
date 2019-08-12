start:
	yarn react-native start
link:
	yarn react-native link && \
		cd ./ios && pod install || cd -

sim.ios: 
	@./scripts/starter ios start

sim.android:
	@./scripts/starter android start Nexus_5X_API_28

test:
	yarn test


android: sim.android
ios: sim.ios

.PHONY: assets
