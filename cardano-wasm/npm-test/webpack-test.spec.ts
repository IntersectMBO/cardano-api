import { test, expect } from '@playwright/test';

test('test output matches', async ({ page }) => {
  const consolePromise = page.waitForEvent('console', {
    predicate: (msg) => msg.text().includes('> "New Transaction Body Created:"')
  });

  await page.goto('http://localhost:8081');

  await consolePromise;
});
