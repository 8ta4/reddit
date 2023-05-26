# reddit

## Introduction

> So, what's this tool all about?

This tool is a game-changer for Reddit marketing. It helps you find Reddit posts that talk about a problem your product can solve. Just mark a post from the past, and the tool will keep an eye on new similar posts. When it finds one, it'll let you know so you can jump in and show off your product.

The main motivation for this is that when I see old social media comments about a problem my product solves, I wish I could go back in time and sell my product to those users. What I've learned is that this is an NP-hard problem known as the time-traveling salesman problem.

> How does it work?

It uses fancy stuff like natural language processing and machine learning to understand Reddit posts and how they relate to your product. You can mark posts as positive or negative examples to teach the tool what you're looking for. The tool will then score the posts based on their similarity to your examples and notify you when there's a match.

## Installation

> How do I install the tool?

The tool comes as a Nix package, so you don't have to worry about dependencies. Just make sure you have Nix installed on your system. If you don't, you can download and install it from the [Nix download page](https://nixos.org/download.html).

Once you've got Nix, just run this command in your terminal to install the Reddit tool:

```
$ nix-env -iA TODO
```

> Why use a Nix package?

Nix packages are awesome because they take care of all dependencies for you, making installation a breeze. Plus, Nix offers atomic upgrades and rollbacks, so you can easily switch between different versions of the Reddit tool without any issues.

## Marking Positive and Negative Examples

> How do I mark a Reddit post as a positive or negative example?

You'll need to create or modify a YAML file to mark a post. The file should look like this:

```
{{topic}}:
  training:
    {{URL}}:
      text: |
        {{text}}
      score: {{score}}
  threshold: {{threshold}}
```

Replace `{{topic}}`, `{{URL}}`, `{{text}}`, `{{score}}`, and `{{threshold}}` with the right values for the post you want to mark. For example:

```
example_topic:
  training:
    https://www.reddit.com/r/example/comments/123abc/some_post_title/:
      text: |
        Post content here...
      score: 0.9
  threshold: 0.8
```

> So, where should I put the YAML configuration file?

You'll want to store it at `~/.config/reddit/config.yaml`. This location is pretty standard for configuration files and will make it easy for you to find and manage.

> Can I mark multiple positive and negative examples for the same topic?

Absolutely! Just add more entries under the `training` section in the YAML file for the same topic. Make sure to include the URL, text, and score for each example. It'll look something like this:

```
example_topic:
  training:
    https://www.reddit.com/r/example/comments/123abc/some_post_title/:
      text: |
        Post content here...
      score: 0.9
    https://www.reddit.com/r/example/comments/456def/another_post_title/:
      text: |
        Another post content...
      score: 0.2
  threshold: 0.8
```

The tool will use all these examples to train the model and find similar posts for you.

## Sourcing Similar Posts

> How does the tool find similar posts?

It uses cosine similarity to compare the text of the positive examples with the text of the new posts.

## Real-time Notifications

> How will the tool notify me of similar posts?

In the MVP, the tool won't send real-time notifications like email or in-app notifications. Instead, it'll add similar posts to a file. You can set up your own watch tools to keep an eye on this file and notify you when there are changes. That way, you can customize your notifications and still stay in the loop.

> How can I set up a watch tool to keep an eye on the file?

1. Pick a file watch tool that works for you, like [fswatch](https://emcrisostomo.github.io/fswatch/) for macOS or [inotify-tools](https://github.com/inotify-tools/inotify-tools/wiki) for Linux.
2. Install your chosen file watch tool, following its documentation.
3. Configure the tool to monitor the file where similar posts are added.
4. Set up a notification method within the watch tool, like sending an email or showing a desktop notification when the file changes.
