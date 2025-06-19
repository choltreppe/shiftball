plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.android)
}

android {
    namespace = "{{ApplicationId}}"
    compileSdk = {{AndroidApiVersion.b}}

    defaultConfig {
        applicationId = "{{ApplicationId}}"
        minSdk = {{AndroidApiVersion.a}}
        targetSdk = {{AndroidApiVersion.b}}
        versionCode = {{AppVersionCode}}
        versionName = "{{AppVersionName}}"

        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
    }

    buildTypes {
        release {
            isMinifyEnabled = false
        }
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_11
        targetCompatibility = JavaVersion.VERSION_11
    }
    kotlinOptions {
        jvmTarget = "11"
    }
    buildFeatures {
        prefab = true
    }
}