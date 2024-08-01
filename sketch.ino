#include <Wire.h>
#include <LiquidCrystal_I2C.h>
#include "DHT.h"
#include <Adafruit_GFX.h>
#include <Adafruit_LEDBackpack.h>

#define DHTPIN 15
#define DHTTYPE DHT22

const int nitrogenPin = 32;
const int phosphorusPin = 33;
const int magnesiumPin = 34;
const int pHpin = 35;
const int waterPin = 36;
const int salinityPin = 39;
const int ecPin = 25;

DHT dht(DHTPIN, DHTTYPE);
LiquidCrystal_I2C lcd(0x27, 20, 4);
Adafruit_8x8matrix matrix = Adafruit_8x8matrix();

void setup() {
  lcd.begin(20, 4);
  lcd.backlight();
  
  dht.begin();
  Serial.begin(115200);
  matrix.begin(0x70);  // Initialize LED matrix

  lcd.setCursor(0, 0);
  lcd.print("Soil Monitoring");
  lcd.setCursor(0, 1);
  lcd.print("System");
  delay(2000);
  lcd.clear();
}

void loop() {
  int nitrogenValue = analogRead(nitrogenPin);
  int phosphorusValue = analogRead(phosphorusPin);
  int magnesiumValue = analogRead(magnesiumPin);
  int pHValue = analogRead(pHpin);
  int waterValue = analogRead(waterPin);
  int salinityValue = analogRead(salinityPin);
  int ecValue = analogRead(ecPin);

  float temperature = dht.readTemperature();
  float humidity = dht.readHumidity();

  float nitrogen = map(nitrogenValue, 0, 4095, 0, 100);
  float phosphorus = map(phosphorusValue, 0, 4095, 0, 100);
  float magnesium = map(magnesiumValue, 0, 4095, 0, 100);
  float pH = map(pHValue, 0, 4095, 0, 14);
  float water = map(waterValue, 0, 4095, 0, 100);
  float salinity = map(salinityValue, 0, 4095, 0, 100);
  float ec = map(ecValue, 0, 4095, 0, 100);

  Serial.print("Nitrogen: ");
  Serial.print(nitrogen);
  Serial.print("% ");
  Serial.print("Phosphorus: ");
  Serial.print(phosphorus);
  Serial.print("% ");
  Serial.print("Magnesium: ");
  Serial.print(magnesium);
  Serial.print("% ");
  Serial.print("pH: ");
  Serial.print(pH);
  Serial.print(" ");
  Serial.print("Water: ");
  Serial.print(water);
  Serial.print("% ");
  Serial.print("Temp: ");
  Serial.print(temperature);
  Serial.print("C ");
  Serial.print("Humidity: ");
  Serial.print(humidity);
  Serial.print("% ");
  Serial.print("Salinity: ");
  Serial.print(salinity);
  Serial.print("% ");
  Serial.print("EC: ");
  Serial.print(ec);
  Serial.println("%");

  lcd.setCursor(0, 0);
  lcd.print("N:");
  lcd.print(nitrogen);
  lcd.print("% P:");
  lcd.print(phosphorus);
  lcd.print("%");

  lcd.setCursor(0, 1);
  lcd.print("M:");
  lcd.print(magnesium);
  lcd.print("% pH:");
  lcd.print(pH);
  lcd.print(" W:");
  lcd.print(water);
  lcd.print("%");

  lcd.setCursor(0, 2);
  lcd.print("Temp:");
  lcd.print(temperature);
  lcd.print("C Hum:");
  lcd.print(humidity);
  lcd.print("%");

  lcd.setCursor(0, 3);
  lcd.print("Sal:");
  lcd.print(salinity);
  lcd.print("% EC:");
  lcd.print(ec);
  lcd.print("%");

  matrix.clear();
  matrix.setCursor(0, 0);
  matrix.print("N: ");
  matrix.print(nitrogen);
  matrix.writeDisplay();

  delay(2000);
}
