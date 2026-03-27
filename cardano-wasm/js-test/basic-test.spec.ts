import { test, expect } from '@playwright/test';
import { readFileSync, writeFileSync } from 'fs';
import { resolve } from 'path';

const goldenPath = resolve(__dirname, 'basic-test.golden');
const recreateGoldenFiles = !!process.env.RECREATE_GOLDEN_FILES;

test('test output matches', async ({ page }) => {
  // Navigate to the test page
  await page.goto('http://localhost:8080');
  // Wait for the page to load and the window title to be set (it should be "cardano-wasm test")
  await expect(page).toHaveTitle(/cardano-wasm test/);
  // Wait for the test to finish running (we signal this by creating a tag with id "finish-tag" and text "Finished test!")
  await expect(page.locator('#finish-tag')).toHaveText("Finished test!");
  // Check the output of the test against the golden file
  const actual = await page.locator('#test-output').textContent();

  if (recreateGoldenFiles) {
    writeFileSync(goldenPath, actual ?? '', 'utf-8');
    console.log(`Golden file updated: ${goldenPath}`);
  } else {
    const goldenText = readFileSync(goldenPath, 'utf-8');
    expect(actual).toBe(goldenText);
  }
});
