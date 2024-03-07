## ✏️ Description

The purpose of this project is to examine the relationship between Google's [Core Web Vitals metrics](https://web.dev/explore/learn-core-web-vitals) and energy consumption on Android devices.

### 📱 Hardware

- [Raspberry Pi 3 Model B+ with 4GB of RAM](https://www.raspberrypi.com/products/raspberry-pi-3-model-b-plus/)
- [Google Pixel 6]
- [Google Pixel 3]

### 🖥️ Software

The following packages may need to be installed on the Raspberry Pi:

- [Node JS](https://nodejs.org/en/)
- [Python 3](https://www.python.org/downloads/)
- [mitmproxy](https://mitmproxy.org/)
- [ngrok](https://ngrok.com/)
- [adb](https://developer.android.com/studio/command-line/adb)

## 🤔 How to run

This experiment was designed to run on a Raspberry Pi 4 Model B with 4GB of RAM. The Raspberry Pi should be running the latest version of Raspberry Pi OS. To replicate the project, the following steps should be taken:

1. Clone this repository onto the Raspberry Pi
2. Clone the Android Runner in the parent directory of this repository `https://github.com/S2-group/android-runner.git`
3. Install the Android Runner dependencies <br/>
   `cd android-runner && pip3 install -r requirements.txt`
4. Install the python dependencies <br/>`pip3 install -r requirements.txt`
5. Install the node dependencies in the receiver directory <br> `cd receiver && npm install`
6. Create 3 ssh sessions to the Raspberry Pi and run the following:
   1. `mitmproxy -s proxy.py`
   2. `node receiver/app.js`
   3. `ngrok http 3000`
7. The `ngrok` window will give you a https URL. Copy this URL into the `.env` file.
8. Finally you can run the experiment using the following command: <br>
   `sudo python3 android-runner android-runner/examples/batterymanager/config_web.json`

All outputs will be stored in the `android-runner/examples/batterymanager/output` directory.
