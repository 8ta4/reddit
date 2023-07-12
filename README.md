# reddit

## Introduction

> So, what's this tool all about?

This tool is a game-changer for Reddit marketing. It helps you find Reddit posts that talk about a problem your product can solve. Just mark a post from the past, and the tool will keep an eye on new similar posts. When it finds one, it'll let you know so you can jump in and show off your product.

The main motivation for this is that when I see old social media comments about a problem my product solves, I wish I could go back in time and sell my product to those users. What I've learned is that this is an NP-hard problem known as the time-traveling salesman problem.

> How does it work?

It uses fancy stuff like natural language processing and machine learning to understand Reddit posts and how they relate to your product. You can mark posts as examples to teach the tool what you're looking for. The tool will then score the posts based on their similarity to your examples and notify you when there's a match.

## Installation

> How do I install the tool?

You need to install devenv and direnv. Check out the [devenv getting started page](https://devenv.sh/getting-started/#installation) and the [direnv installation guide](https://devenv.sh/automatic-shell-activation/#installing-direnv) to get them set up on your system.

Once you've got devenv and direnv installed, just run these commands:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/8ta4/reddit/main/install.sh)"
cd reddit
```

## Marking Examples

> How do I mark a Reddit post as an example?

You'll need to create or modify a YAML file to mark a post. The file should look like this:

```
https://www.reddit.com/r/example/comments/123abc/some_post_title/:
  text: |
    Post content here...
  threshold: 0.8
```

> So, where should I put the YAML configuration file?

You'll want to store it at `~/.config/reddit/config.yaml`. This location is pretty standard for configuration files and will make it easy for you to find and manage.

> Can I mark multiple examples?

Absolutely! Just add more entries in the YAML file. Make sure to include the URL, text, and threshold for each example. It'll look something like this:

```
https://www.reddit.com/r/example/comments/123abc/some_post_title/:
  text: |
    Post content here...
  threshold: 0.9
https://www.reddit.com/r/example/comments/456def/another_post_title/:
  text: |
    Another post content...
  threshold: 0.2
```

## Using the CLI Tool

> So, how do I actually use this tool to find matching posts?

First, make sure you're in the project root directory. If you're not there, use the `cd` command to navigate to it. Then, just run the `reddit` command.

```bash
reddit
```

> What if I want to see the similarity scores for a specific post?

You can use the `reddit scores` command for that. Just pop in the post's URL after the command, like this:

```bash
reddit scores https://www.reddit.com/r/example/comments/123abc/some_post_title/
```

The tool will show you the similarity scores for that post compared to each example in your config file.

## Sourcing Similar Posts

> How does the tool find similar posts?

It uses cosine similarity to compare the text of the examples with the text of the new posts.

## Real-time Notifications

> How will the tool notify me of similar posts?

In the MVP, the tool won't send real-time notifications like email or in-app notifications. Instead, it'll write similar posts to `stdout`. You can set up your own watch tools to keep an eye on the output and notify you when there are changes. That way, you can customize your notifications and still stay in the loop.

## Updating the Tool

> How do I update the tool?

Just make sure you're in the project root directory. If you're not there, use the `cd` command to navigate to it. Then, run this command:

```bash
git pull
```
